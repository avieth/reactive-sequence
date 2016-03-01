{-|
Module      : Reactive.Sequence
Description : Use Events applicatively.
Copyright   : (c) Alexander Vieth, 2016
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE RecursiveDo #-}

module Reactive.Sequence where

import Control.Monad.IO.Class
import Control.Applicative
import Data.Semigroup
import Data.IORef
import Data.Functor.Compose
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.IO.Unsafe
import Debug.Trace

data Once f t where
    Once :: (IORef (Either (f s) s)) -> (s -> t) -> Once f t

instance Functor (Once f) where
    fmap f (Once ref g) = Once ref (fmap f g)

runOnce :: MonadIO f => Once f t -> f t
runOnce (Once ioref g) = do
    x <- liftIO (readIORef ioref)
    case x of
        Left uncomputed -> do
            computed <- uncomputed
            liftIO $ writeIORef ioref (Right computed)
            pure (g computed)
        Right computed -> pure (g computed)

mkOnce :: MonadIO f => f t -> Once f t
mkOnce uncomputed = unsafePerformIO $ do
    ref <- liftIO $ newIORef (Left uncomputed)
    pure (Once ref id)

once :: MonadIO f => f t -> f t
once = runOnce . mkOnce

-- | Use this type to compute applicatively with reactive-banana events.
newtype Sequence t = Sequence {
      getSequence :: MomentIO (t, Event t)
    }

revent :: Event t -> Compose Sequence Maybe t
revent ev = Compose . Sequence $ do
    pure (Nothing, ev')
  where
    ev' = Just <$> ev

behavior :: Sequence t -> MomentIO (Behavior t, Event t)
behavior seqnc = once $ do
    (first, rest) <- runSequence seqnc
    b <- stepper first rest
    pure (b, rest)

rstepper :: t -> Event t -> Sequence t
rstepper t ev = Sequence $ pure (t, ev)

(|>) :: t -> Event t -> Sequence t
(|>) = rstepper

rstepper' :: t -> Sequence t -> Sequence t
rstepper' t seq = Sequence . once $ do
    (_, ev) <- runSequence seq
    pure (t, ev)

(|~) :: t -> Sequence t -> Sequence t
(|~) = rstepper'

always :: t -> Sequence t
always t = Sequence $ pure (t, never)

-- | Get the sequence lagged one frame.
lag :: Sequence t -> Sequence t
lag seqnc = Sequence . once $ do
    (lagged, fire) <- newEvent
    (t, ev) <- runSequence seqnc
    reactimate (fire <$> ev)
    pure (t, lagged)

-- | Use this to make recursively-defined Sequences. You'll almost certainly
--   use ~*~ in the function.
withInitial :: t -> (Sequence t -> Sequence t) -> Sequence t
withInitial t f = Sequence . once $ mdo
    let b = rstepper t ev
    let e = f b
    (_, ev) <- runSequence e
    pure (t, ev)

runSequence :: Sequence t -> MomentIO (t, Event t)
runSequence = getSequence

-- | Switch between Sequences. Whenever the event fires, the first element of
--   the Sequence is emitted, and its remaining elements follow until the
--   event fires again.
sequenceSwitch' :: forall t . Event t -> Event (Sequence t) -> MomentIO (Event t)
sequenceSwitch' first ev = once $ do
    secondHasFired :: Behavior Bool <- stepper False (const True <$> ev)
    let controlledFirst :: Event t
        controlledFirst = whenE (not <$> secondHasFired) first
    executed :: Event (t, Event t) <- execute (runSequence <$> ev)
    {-
    let firsts :: Event t
        firsts = unionWith const (fst <$> executed) controlledFirst 
    let rests :: Event t
        rests = switchE (snd <$> executed)
    pure (unionWith const firsts rests)
    -}
    let bundle :: forall t . Event (t, Event t) -> Event t
        bundle ev = unionWith const (switchE (snd <$> ev)) (fst <$> ev)
    let bundled :: Event t
        bundled = bundle executed
    pure (unionWith const bundled controlledFirst)

-- | This is like sequenceSwitch' but we have to account for the initial
--   sequence. How?! How to determine when to ditch the initial sequence's
--   event? Could make a new event... seems sloppy though.
--   If we can get two Event (Sequence t)s then we're golden.
--
--   Must be very careful about the semantics here.
--   Whenever the sequence changes, the output sequence assumes its sequence.
--   We could do this by making a new event, but I'd rather it be immediate.
--
--
sequenceSwitch :: forall t . Sequence (Sequence t) -> Sequence t
sequenceSwitch seqnc = Sequence . once $ do
    -- laterSequences is in control. Whenever it fires, we want to put out
    -- its first element and switch to its event.
    -- Ultimately what we must do is come up with an Event (Event t).
    -- We will need the Event from the firstSequence to be one of these, but
    -- I don't believe reactive-banana provides a canonical way to do this.
    -- Ok, we can create a new event and fire it right away.
    (firstSequence :: Sequence t, laterSequences :: Event (Sequence t))
        <- runSequence seqnc
    (first :: t, firstRest :: Event t) <- runSequence firstSequence
    switched :: Event t <- sequenceSwitch' firstRest laterSequences
    pure (first, switched)

sequenceSwitchE :: forall t . Sequence (Event t) -> MomentIO (Event t)
sequenceSwitchE seqnc = do
    (firstEv, restEv) <- runSequence seqnc
    restHasFired :: Behavior Bool <- stepper False (const True <$> restEv)
    let first :: Event t
        first = whenE (not <$> restHasFired) firstEv
    let rest :: Event t
        rest = switchE restEv
    pure (unionWith const rest first)

sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate seqnc = do
    (first, rest) <- runSequence seqnc
    liftIO first
    reactimate rest

sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute seqnc = once $ do
    (first, rest) <- runSequence seqnc
    t <- first
    ev <- execute rest
    pure (rstepper t ev)

-- | Like sequenceCommute, but it's performed when the Sequence is forced.
--   TBD useful at all? Seems simply to not work when you expect it to.
sequenceAbsorb :: Sequence (MomentIO t) -> Sequence t
sequenceAbsorb seqnc = Sequence $ do
    (first, rest) <- runSequence seqnc
    t <- first
    ev <- execute rest
    pure (t, ev)

instance Functor Sequence where
    fmap f = Sequence . fmap f' . getSequence
      where
        f' ~(t, ev) = (f t, fmap f ev)

instance Applicative Sequence where
    pure x = Sequence $ pure (x, never)
    (mf :: Sequence (s -> t)) <*> (mx :: Sequence s) = Sequence . once $ do
        (firstf, evf) <- getSequence $ mf
        (firstx, evx) <- getSequence $ mx
        combined <- combineIt (firstf, evf) (firstx, evx)
        pure (firstf firstx, combined)
      where
        combineIt (firstf, evf) (firstx, evx) = do
            bef <- stepper firstf evf
            bex <- stepper firstx evx
            let xchange :: Event (s -> t, s)
                xchange = (,) <$> bef <@> evx
            let fchange :: Event (s -> t, s)
                fchange = (flip (,)) <$> bex <@> evf
            -- We must carefully handle the case in which evf and evx fire at
            -- precisely the same moment. When that happens, we ignore the
            -- behaviors.
            let unioner :: (s -> t, s) -> (s -> t, s) -> (s -> t, s)
                unioner (f, _) (_, x) = (f, x)
            let unioned :: Event (s -> t, s)
                unioned = unionWith unioner fchange xchange
            let applied :: Event t
                applied = fmap (uncurry ($)) unioned
            pure applied

{-
   Recursion in reactive-banana comes up all the time. The example from the
   Hackage docs:

     let e2 = (\a f -> f a) <$> b <@> e1
     b <- stepper a e2
     return e2

   The applicative instance for Sequence can't be used in this way, because
   it uses the events of both arguments. If we write

     let e2 = (\a f -> f a) <$> b <*> e1
     let b = rstepper a e2
     runSequence e2

   then we have a useless value: the event in b fires whenever the event in
   e2 fires, which in turn fires whenever the event in e1 *or the event in b*
   fires. We've created a cycle.

   The right way to express this is so:

     let e2 = e1 ~%~ b
     let b = rstepper a e2
     runSequence e2

   The combinator ~%~, an alias for lazyAp, is just like <*> except that its
   second parameter's event is used only to come up with a stepper. The
   resulting Sequence's event does not fire when its second argument's
   event fires.
-}

class LazyApplicative f where
    -- | A lazy variant of <*>, which uses the events of the RHS only as an argument
    --   to stepper.
    lazyAp :: f (s -> t) -> f s -> f t

infixl 4 ~*~
(~*~) :: LazyApplicative f => f (s -> t) -> f s -> f t
(~*~) = lazyAp

instance LazyApplicative Sequence where
    lazyAp mf mx = Sequence . once $ do
        (firstf, evf) <- getSequence $ mf
        (firstx, evx) <- getSequence $ mx
        bef <- stepper firstf evf
        bex <- stepper firstx evx
        let applied = (\x f -> f x) <$> bex <@> evf
        pure (firstf firstx, applied)

instance Applicative f => LazyApplicative (Compose Sequence f) where
    lazyAp mf mx = Compose $ (<*>) <$> getCompose mf ~*~ getCompose mx

newtype SemigroupEvent t = SemigroupEvent {
      runSemigroupEvent :: Event t
    }

deriving instance Functor SemigroupEvent

instance
    ( Semigroup t
    ) => Semigroup (SemigroupEvent t)
  where
    left <> right = SemigroupEvent $ unionWith (<>) (runSemigroupEvent left) (runSemigroupEvent right)

instance
    ( Semigroup t
    ) => Semigroup (Sequence t)
  where
    left <> right = Sequence . once $ do
        (firstleft, evleft) <- getSequence left
        (firstright, evright) <- getSequence right
        let first = firstleft <> firstright
        let ev = unionWith (<>) evleft evright
        pure (first, ev)

-- If t is a Semigroup then SemigroupEvent t is a Monoid, because never can
-- serve as the identity.
instance
    ( Semigroup t
    ) => Monoid (SemigroupEvent t)
  where
    mempty = SemigroupEvent never
    mappend = (<>)

-- This one overlaps with one from Data.Functor.Compose
--
--   Alternative f, Applicative g => Alternative (Compose f g)
--
-- but since Sequence is not and never will be an Alternative, that instance
-- can never match.
instance {-# OVERLAPS #-} Alternative f => Alternative (Compose Sequence f) where
    empty = Compose (pure empty)
    (Compose (left :: Sequence (f t))) <|> (Compose (right :: Sequence (f t))) = Compose . Sequence . once $ do
        (firstleft, evleft) <- getSequence $ left
        (firstright, evright) <- getSequence $ right
        let first = firstleft <|> firstright
        let ev = unionWith (<|>) evleft evright
        pure (first, ev)
