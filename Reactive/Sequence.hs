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
{-# LANGUAGE RankNTypes #-}

module Reactive.Sequence (

      Sequence
    , runSequence
    , behavior
    , revent
    , rstepper
    , (|>)
    , always
    , withInitial
    , sequenceSwitchE
    , sequenceSwitch
    , sequenceReactimate
    , sequenceCommute
    , LazyApplicative
    , (~*~)
    , lazyAp
    , SemigroupEvent(..)
    , sequenceAccumulate

    ) where

import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.MVar
import Data.Semigroup
import Data.Functor.Compose
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.IO.Unsafe

{-# NOINLINE once #-}
once :: forall m t . Monad m => m t -> m t
once uncomputed =
    let {-# NOINLINE mvar #-}
        mvar :: MVar (Either (m t) t)
        mvar = unsafePerformIO $ newMVar (Left uncomputed)
        {-# NOINLINE check #-}
        check :: Either (m t) t
        check = unsafePerformIO $ takeMVar mvar
        {-# NOINLINE replace #-}
        replace :: t -> ()
        replace t = unsafePerformIO (putMVar mvar (Right t))
    in  case check of
            Left uncomputed -> do
                t <- uncomputed
                pure (replace t `seq` t)
            Right computed -> pure (replace computed `seq` computed)

-- | A time-varying value of type t: a value and an event giving changes.
--   Think of it as a formal version of stepper.
newtype Sequence t = Sequence {
      runSequence :: Moment (t, Event t)
    }

getSequence :: MonadMoment m => Sequence t -> m (t, Event t)
getSequence = liftMoment . runSequence

instance Functor Sequence where
    fmap f = Sequence . fmap f' . runSequence
      where
        f' (t, ev) = (f t, fmap f ev)

instance Applicative Sequence where
    pure x = Sequence $ pure (x, never)
    (mf :: Sequence (s -> t)) <*> (mx :: Sequence s) = Sequence . once $ do
        ~(firstf, evf) <- runSequence mf
        ~(firstx, evx) <- runSequence mx
        combined <- combineIt (firstf, evf) (firstx, evx)
        pure (firstf firstx, combined)
      where
        combineIt ~(firstf, evf) ~(firstx, evx) = do
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

revent :: Event t -> Compose Sequence Maybe t
revent ev = Compose . Sequence $ pure (Nothing, ev')
  where
    ev' = Just <$> ev

behavior :: Sequence t -> Moment (Behavior t, Event t)
behavior seqnc = do
    ~(first, rest) <- runSequence seqnc
    b <- stepper first rest
    pure (b, rest)

rstepper :: t -> Event t -> Sequence t
rstepper t ev = Sequence $ pure (t, ev)

(|>) :: t -> Event t -> Sequence t
(|>) = rstepper

always :: t -> Sequence t
always t = Sequence $ pure (t, never)

-- | Use this to make recursively-defined Sequences. You'll almost certainly
--   use ~*~ in the function.
withInitial
    :: t
    -> (Sequence t -> Sequence t)
    -> Sequence t
withInitial t f = Sequence $ mdo
    let b = rstepper t ev
    let e = f b
    ~(_, ev) <- runSequence e
    pure (t, ev)

sequenceSwitchE
    :: forall t .
       Sequence (Event t)
    -> Moment (Event t)
sequenceSwitchE seqnc = do
    ~(firstEv, restEv) <- runSequence seqnc
    restHasFired :: Behavior Bool <- stepper False (const True <$> restEv)
    let first :: Event t
        first = whenE (not <$> restHasFired) firstEv
    let rest :: Event t
        rest = switchE restEv
    pure (unionWith const rest first)

-- | Switch between Sequences. Whenever the event fires, the first element of
--   the Sequence is emitted, and its remaining elements follow until the
--   event fires again.
sequenceSwitch'
    :: forall t .
       Event t
    -> Event (Sequence t)
    -> Moment (Event t)
sequenceSwitch' first ev = do
    secondHasFired :: Behavior Bool <- stepper False (const True <$> ev)
    let controlledFirst :: Event t
        controlledFirst = whenE (not <$> secondHasFired) first
    let observed :: Event (t, Event t)
        observed = observeE (runSequence <$> ev)
    let bundle :: forall t . Event (t, Event t) -> Event t
        bundle ev = unionWith const (switchE (snd <$> ev)) (fst <$> ev)
    let bundled :: Event t
        bundled = bundle observed
    pure (unionWith const bundled controlledFirst)

-- | This is like sequenceSwitch' but we have to account for the initial
--   sequence. How?! How to determine when to ditch the initial sequence's
--   event? Could make a new event... seems sloppy though.
--   If we can get two Event (Sequence t)s then we're golden.
--
--   Must be very careful about the semantics here.
--   Whenever the sequence changes, the output sequence assumes its sequence.
--   We could do this by making a new event, but I'd rather it be immediate.
sequenceSwitch
    :: forall t .
       Sequence (Sequence t)
    -> Sequence t
sequenceSwitch seqnc = Sequence . once $ do
    (firstSequence :: Sequence t, laterSequences :: Event (Sequence t))
        <- runSequence seqnc
    (first :: t, firstRest :: Event t) <- runSequence firstSequence
    switched :: Event t <- sequenceSwitch' firstRest laterSequences
    pure (first, switched)

sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate seqnc = do
    (first, rest) <- liftMoment (runSequence seqnc)
    liftIO first
    reactimate rest

sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute seqnc = do
    (first, rest) <- liftMoment (runSequence seqnc)
    t <- first
    ev <- execute rest
    pure (rstepper t ev)


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
        ~(firstf, evf) <- runSequence mf
        ~(firstx, evx) <- runSequence mx
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
        ~(firstleft, evleft) <- runSequence left
        ~(firstright, evright) <- runSequence right
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
instance {-# OVERLAPS #-}
    ( Alternative f
    ) => Alternative (Compose Sequence f)
  where
    empty = Compose (pure empty)
    (Compose (left :: Sequence (f t))) <|> (Compose (right :: Sequence (f t))) = Compose . Sequence . once $ do
        ~(firstleft, evleft) <- runSequence left
        ~(firstright, evright) <- runSequence right
        let first = firstleft <|> firstright
        let ev = unionWith (<|>) evleft evright
        pure (first, ev)

-- | Given a first value, an event with changes, and a way to combine them,
--   get a Sequence with the latest value. It changes whenever the event
--   changes.
sequenceAccumulate
    :: (b -> a -> a)
    -> (a, Event b)
    -> Sequence a
sequenceAccumulate f (a, ev) = Sequence . once $ mdo
    let changes = flip f <$> be <@> ev
    be <- stepper a changes
    pure (a, changes)
