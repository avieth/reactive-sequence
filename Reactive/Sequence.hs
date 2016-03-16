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

      SemigroupEvent(..)
    , Sequence
    , runSequence
    , getSequence
    , sequenceFirst
    , sequenceEvent
    , sequenceBehavior
    , sequenceChanges
    , revent
    , rstepper
    , (|>)
    , always
    , sequenceSwitchE
    , sequenceSwitch
    , sequenceAccumulate
    , sequenceReactimate
    , sequenceCommute
    , LazyApplicative
    , (~*~)
    , lazyAp

    ) where

import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.MVar
import Data.Semigroup
import Data.Functor.Compose
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.IO.Unsafe

-- | A time-varying value of type t: a value and an event giving changes.
--   Think of it as a formal version of stepper.
newtype Sequence t = Sequence {
      runSequence :: Moment (t, Event t)
    }

-- | Like runSequence but in an arbitrary MonadMoment, for convenience.
getSequence :: MonadMoment m => Sequence t -> m (t, Event t)
getSequence ~(Sequence m) = liftMoment m

instance Functor Sequence where
    fmap f = Sequence . fmap f' . runSequence
      where
        f' ~(t, ev) = (f t, fmap f ev)

instance Applicative Sequence where
    pure x = Sequence $ pure (x, never)
    (mf :: Sequence (s -> t)) <*> (mx :: Sequence s) = Sequence $ do
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

-- | Make a Sequence from an Event, by giving Nothing for the initial value.
revent :: Event t -> Compose Sequence Maybe t
revent ev = Compose . Sequence $ pure (Nothing, ev')
  where
    ev' = Just <$> ev

-- | Get the initial value of a Sequence.
sequenceFirst :: MonadMoment m => Sequence t -> m t
sequenceFirst seqnc = do
    ~(first, _) <- getSequence seqnc
    pure first

-- | Get the changes to a Sequence.
sequenceEvent :: MonadMoment m => Sequence t -> m (Event t)
sequenceEvent seqnc = liftMoment $ do
    ~(_, rest) <- runSequence seqnc
    pure rest

-- | Derive a Behavior from a Sequence.
sequenceBehavior :: MonadMoment m => Sequence t -> m (Behavior t)
sequenceBehavior seqnc = do
    ~(first, rest) <- getSequence seqnc
    b <- stepper first rest
    pure b

-- | Derive an Event which fires whenever the Sequence changes, pairing the
--   new value (snd) with the previous value (fst).
sequenceChanges :: MonadMoment m => Sequence t -> m (Event (t, t))
sequenceChanges seqnc = do
    be <- sequenceBehavior seqnc
    ev <- sequenceEvent seqnc
    pure $ (,) <$> be <@> ev

-- | Construct a Sequence by giving an initia value and its changes.
--   Compare at @stepper :: MonadMoment m => t -> Event t -> m (Behavior t)@
rstepper :: t -> Event t -> Sequence t
rstepper t ev = Sequence $ pure (t, ev)

(|>) :: t -> Event t -> Sequence t
(|>) = rstepper

-- | A Sequence which never changes. This is @pure@ in the Sequence Applicative
--   instance.
always :: t -> Sequence t
always t = Sequence $ pure (t, never)

-- | Switch a Sequence of Events. The derived Event fires when the latest Event
--   to come from the Sequence fires.
sequenceSwitchE
    :: forall t m .
       ( MonadMoment m )
    => Sequence (Event t)
    -> m (Event t)
sequenceSwitchE seqnc = do
    ~(firstEv, restEv) <- getSequence seqnc
    restHasFired :: Behavior Bool <- stepper False (const True <$> restEv)
    let first :: Event t
        first = whenE (not <$> restHasFired) firstEv
    let rest :: Event t
        rest = switchE restEv
    pure (unionWith const rest first)

sequenceSwitch'
    :: forall t m .
       ( MonadMoment m )
    => Event t
    -> Event (Sequence t)
    -> m (Event t)
sequenceSwitch' first ev = liftMoment $ do
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

-- | Switch between Sequences. Whenever the event fires, the first element of
--   the Sequence is emitted, and its remaining elements follow until the
--   event fires again.
sequenceSwitch
    :: forall t .
       Sequence (Sequence t)
    -> Sequence t
sequenceSwitch seqnc = Sequence $ do
    ~(firstSequence :: Sequence t, laterSequences :: Event (Sequence t))
        <- runSequence seqnc
    ~(first :: t, firstRest :: Event t) <- runSequence firstSequence
    switched :: Event t <- sequenceSwitch' firstRest laterSequences
    pure (first, switched)

-- | Given a first value, an event with changes, and a way to combine them,
--   get a Sequence with the latest value. It changes whenever the event
--   changes.
sequenceAccumulate
    :: (b -> a -> a)
    -> a
    -> Event b
    -> Sequence a
sequenceAccumulate f a ev = Sequence $ mdo
    -- Using Sequence . once causes divergence in some cases. Not sure why.
    let changes = flip f <$> be <@> ev
    be <- stepper a changes
    pure (a, changes)

-- | Run an IO whenever the Sequence produces a new one. The initial one is
--   run immediately.
sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate seqnc = do
    (first, rest) <- liftMoment (runSequence seqnc)
    liftIO first
    reactimate rest

-- | Like execute :: Event (MomentIO t) -> MomentIO (Event t).
--   The initial MomentIO is run immediately.
sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute seqnc = do
    (first, rest) <- liftMoment (runSequence seqnc)
    t <- first
    ev <- execute rest
    pure (rstepper t ev)

class LazyApplicative f where
    -- | A lazy variant of <*>, which uses the events of the RHS only as an argument
    --   to stepper.
    lazyAp :: f (s -> t) -> f s -> f t

infixl 4 ~*~
(~*~) :: LazyApplicative f => f (s -> t) -> f s -> f t
(~*~) = lazyAp

instance LazyApplicative Sequence where
    lazyAp mf mx = Sequence $ do
        ~(firstf, evf) <- runSequence mf
        ~(firstx, evx) <- runSequence mx
        bex <- stepper firstx evx
        let applied = (\x f -> f x) <$> bex <@> evf
        pure (firstf firstx, applied)

instance Applicative f => LazyApplicative (Compose Sequence f) where
    lazyAp mf mx = Compose $ (<*>) <$> getCompose mf ~*~ getCompose mx

-- | If an Event carries a Semigroup, then it's a Monoid: never is the
--   identity, and unionWith (<>) combines two values.
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
    ) => Monoid (SemigroupEvent t)
  where
    mempty = SemigroupEvent never
    mappend = (<>)

instance
    ( Semigroup t
    ) => Semigroup (Sequence t)
  where
    left <> right = Sequence $ do
        ~(firstleft, evleft) <- runSequence left
        ~(firstright, evright) <- runSequence right
        let first = firstleft <> firstright
        let ev = unionWith (<>) evleft evright
        pure (first, ev)

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
    (Compose (left :: Sequence (f t))) <|> (Compose (right :: Sequence (f t))) = Compose . Sequence $ do
        ~(firstleft, evleft) <- runSequence left
        ~(firstright, evright) <- runSequence right
        let first = firstleft <|> firstright
        let ev = unionWith (<|>) evleft evright
        pure (first, ev)
