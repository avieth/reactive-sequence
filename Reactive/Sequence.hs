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
    , Eventually(..)
    , eventually
    , immediately
    , delayed
    , toSequence

    , Sequence(..)
    , SequenceBuilder(..)
    , sequenceBuilder
    , buildSequence
    , sequenceFirst
    , sequenceEvent
    , sequenceBehavior
    , sequenceChanges
    , rstepper
    , (|>)
    , always
    , sequenceSwitchE
    , sequenceSwitch
    , sequenceAccumulate
    , sequenceReactimate
    , sequenceCommute

    ) where

import Control.Monad.IO.Class
import Control.Applicative
import Control.Concurrent.MVar
import Data.Semigroup
import Data.Functor.Compose
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import System.IO.Unsafe

newtype Eventually t = Eventually {
      getEventually :: Either t (Event t)
    }

instance Functor Eventually where
    fmap f (Eventually choice) = Eventually $ case choice of
        Left t -> Left (f t)
        Right e -> Right (f <$> e)

eventually :: (t -> r) -> (Event t -> r) -> Eventually t -> r
eventually l r = either l r . getEventually

immediately :: t -> Eventually t
immediately = Eventually . Left

delayed :: Event t -> Eventually t
delayed = Eventually . Right

toSequence :: Monoid t => Eventually t -> Sequence t
toSequence = eventually always (\e -> mempty |> e)

-- | A time-varying value of type t: a value and an event giving changes.
--   Think of it as a formal version of stepper.
newtype Sequence t = Sequence {
      runSequence :: (t, Event t)
    }

instance Functor Sequence where
    fmap f ~(Sequence (t, ev)) = Sequence (f t, fmap f ev)

-- | Like a Sequence, but inside Moment, allowing applicative-style programming.
--   Beware: unsuitable for lazy use. Run it to a Sequence before passing it
--   back.
newtype SequenceBuilder t = SequenceBuilder {
      runSequenceBuilder :: Moment (Sequence t)
    }

sequenceBuilder :: Sequence t -> SequenceBuilder t
sequenceBuilder = SequenceBuilder . pure

buildSequence :: MonadMoment m => SequenceBuilder t -> m (Sequence t)
buildSequence = liftMoment . runSequenceBuilder

instance Functor SequenceBuilder where
    fmap f = SequenceBuilder . (fmap . fmap) f . runSequenceBuilder

instance Applicative SequenceBuilder where
    pure x = SequenceBuilder (pure (Sequence (x, never)))
    (mf :: SequenceBuilder (s -> t)) <*> (mx :: SequenceBuilder s) = SequenceBuilder $ do
        ~(firstf, evf) <- runSequence <$> runSequenceBuilder mf
        ~(firstx, evx) <- runSequence <$> runSequenceBuilder mx
        combined <- combineIt (firstf, evf) (firstx, evx)
        pure (Sequence (firstf firstx, combined))
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

-- | Get the initial value of a Sequence.
sequenceFirst :: Sequence t -> t
sequenceFirst ~(Sequence (t, _)) = t

-- | Get the changes to a Sequence.
sequenceEvent :: Sequence t -> Event t
sequenceEvent ~(Sequence (_, ev)) = ev

-- | Derive a Behavior from a Sequence.
sequenceBehavior :: MonadMoment m => Sequence t -> m (Behavior t)
sequenceBehavior seqnc = do
    let first = sequenceFirst seqnc
    let rest = sequenceEvent seqnc
    stepper first rest

-- | Derive an Event which fires whenever the Sequence changes, pairing the
--   new value (snd) with the previous value (fst).
sequenceChanges :: MonadMoment m => Sequence t -> m (Event (t, t))
sequenceChanges seqnc = do
    let ev = sequenceEvent seqnc
    be <- sequenceBehavior seqnc
    pure $ (,) <$> be <@> ev

-- | Construct a Sequence by giving an initia value and its changes.
--   Compare at @stepper :: MonadMoment m => t -> Event t -> m (Behavior t)@
rstepper :: t -> Event t -> Sequence t
rstepper t ev = Sequence (t, ev)

(|>) :: t -> Event t -> Sequence t
(|>) = rstepper

-- | A Sequence which never changes. This is @pure@ in the Sequence Applicative
--   instance.
always :: t -> Sequence t
always t = Sequence (t, never)

-- | Switch a Sequence of Events. The derived Event fires when the latest Event
--   to come from the Sequence fires.
sequenceSwitchE
    :: forall t m .
       ( MonadMoment m )
    => Sequence (Event t)
    -> m (Event t)
sequenceSwitchE ~(Sequence (firstEv, restEv)) = do
    restHasFired :: Behavior Bool <- stepper False (const True <$> restEv)
    let first :: Event t
        first = whenE (not <$> restHasFired) firstEv
    rest :: Event t
        <- switchE restEv
    pure (unionWith const rest first)

{-
sequenceSwitchE'
    :: forall t m .
       ( MonadMoment m )
    => Event (Sequence t)
    -> m (Event t)
sequenceSwitchE' ev = do
    let observed :: Event (t, Event t)
        observed = observeE (runSequence <$> ev)
    -- We have here two sources of events: those which come from the initial
    -- parts, and those which come from the event parts.
    let initialParts :: Event t
        initialParts = fst <$> observed
    let eventParts :: Event (Event t)
        eventParts = snd <$> observed
    -- Whenever a new initial part is heard, we want to stop whatever event
    -- is currently in focus, and shift over to the event associated with that
    -- initial part.
    -- If we switchE the events parts, then we lose the ability to distinguish
    -- between events belonging to different initial parts. We need the outer
    -- event.
    -- To sort this all out we use the notion of an alternator; two of them, in
    -- fact. When they are equal, we let the switched event parts through, but
    -- a firing of initial will flip the initial alternator, making them not
    -- equal. While they're not equal, a firing of eventParts will flip the
    -- event alternator, making them equal again.
    -- eventParts ought never to fire twice without initialParts firing in
    -- between. by construction of Sequence.
    let switched = switchE eventParts
    alternatorInitial :: Behavior Bool
        <- stepper True alternateInitial
    let alternateInitial :: Event Bool
        alternateInitial = not <$> alternateInitial (alternatorEvent <@ initialParts)
    alternatorEvent :: Behavior Bool
        <- stepper True alternateEvent
    let alternateEvent :: Event Bool
        alternateEvent = not <$> alternateEvent <@> eventParts

    pure (whenE sync switched)
-}

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
        observed = runSequence <$> ev
    let bundle :: forall t . Event (t, Event t) -> Moment (Event t)
        bundle ev = do
            switched <- switchE (snd <$> ev)
            pure (unionWith const switched (fst <$> ev))
    bundled :: Event t
        <- bundle observed
    pure (unionWith const bundled controlledFirst)

-- | Switch between Sequences. Whenever the event fires, the first element of
--   the Sequence is emitted, and its remaining elements follow until the
--   event fires again.
sequenceSwitch
    :: forall t .
       Sequence (Sequence t)
    -> SequenceBuilder t
sequenceSwitch ~(Sequence (firstSequence, laterSequences)) = SequenceBuilder $ do
    let ~(first :: t, firstRest :: Event t) = runSequence firstSequence
    switched :: Event t <- sequenceSwitch' firstRest laterSequences
    pure (Sequence (first, switched))

-- | Given a first value, an event with changes, and a way to combine them,
--   get a Sequence with the latest value. It changes whenever the event
--   changes.
sequenceAccumulate
    :: (b -> a -> a)
    -> a
    -> Event b
    -> SequenceBuilder a
sequenceAccumulate f a ev = SequenceBuilder $ mdo
    -- Using Sequence . once causes divergence in some cases. Not sure why.
    let changes = flip f <$> be <@> ev
    be <- stepper a changes
    pure (Sequence (a, changes))

-- | Run an IO whenever the Sequence produces a new one. The initial one is
--   run immediately.
sequenceReactimate :: Sequence (IO ()) -> MomentIO ()
sequenceReactimate seqnc = do
    let (first, rest) = runSequence seqnc
    liftIO first
    reactimate rest

-- | Like execute :: Event (MomentIO t) -> MomentIO (Event t).
--   The initial MomentIO is run immediately.
sequenceCommute :: Sequence (MomentIO t) -> MomentIO (Sequence t)
sequenceCommute seqnc = do
    let (first, rest) = runSequence seqnc
    t <- first
    ev <- execute rest
    pure (rstepper t ev)

{-
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
-}

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
    ~(Sequence (firstleft, evleft)) <> ~(Sequence (firstright, evright)) =
        let first = firstleft <> firstright
            ev = unionWith (<>) evleft evright
        in  Sequence (first, ev)

instance
    ( Semigroup t
    ) => Semigroup (SequenceBuilder t)
  where
    left <> right = SequenceBuilder $ do
        l <- runSequenceBuilder left
        r <- runSequenceBuilder right
        pure (l <> r)

{-
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
        -}
