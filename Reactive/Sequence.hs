{-|
Module      : Reactive.Sequence
Description : Definition of Sequence, which generalizes and unifies the
              Reactive.Banana Event and Behavior types.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE RecursiveDo #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE UnicodeSyntax #-}
{-# LANGUAGE UndecidableInstances #-}

module Reactive.Sequence (
      SequenceM
    , runSequenceM
    , Sequence(..)
    , SValue
    , SEvent
    , SBehavior
    , sequenceTrans
    , (|>)
    , always
    , nothing
    , lag
    , lag'
    , sEventToSBehavior
    , sBehaviorToSEvent
    , eventToSEvent
    , sEventToEvent
    , sBehaviorToBehavior
    , fixSBehavior
    , bundle
    , Bundleable
    , bundle'
    , bundleLeft'
    , bundleRight'
    , (<%>)
    , (<⌚>)
    , (<%)
    , (%>)
    , sequenceUnion
    , Unionable
    , UnionsTo
    , sequenceUnion'
    , (<||>)
    , sequenceCommute
    , switchSequence
    , Switchable
    , SwitchesTo
    , switchSequence'
    , switch
    , sequenceReactimate
    , immediatelyAfter
    ) where

import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Void
import Data.Proxy
import Data.Functor.Identity
import Data.EitherBoth
import Data.Semigroup
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'

-- | TODO use this instead of MomentIO in the sequence combinators. Useful to
--   identify a subset of MomentIO which can't do just anything (this is not
--   MonadIO, and its constructor is not exported).
newtype SequenceM t = SequenceM {
      outSequenceM :: MomentIO t
    }

runSequenceM :: SequenceM t -> MomentIO t
runSequenceM = outSequenceM

deriving instance Functor SequenceM
deriving instance Applicative SequenceM
deriving instance Monad SequenceM
deriving instance MonadFix SequenceM
deriving instance MonadMoment SequenceM

-- | A @Sequence@ is like an @Event@, and also like a @Behavior@. It's
--   one functorial value, followed by an @Event@ bringing more functorial
--   values through a possibly different functor. By modulating these
--   parameters we uncover types which resemble @Event@ and @Behavior@:
--
--     @
--       Sequence (Const ()) Identity ~ Event
--       Sequence Identity Identity ~ Behavior
--     @
--
data Sequence (f :: * -> *) (g :: * -> *) (t :: *) where
    Sequence
        :: MomentIO ( f s                      -- Initial value
                    , MomentIO (Event (g s))   -- Remaining values
                    , Event (g s)              -- Remaining values, lagged one frame
                    )
        -> (s -> t)
        -> Sequence f g t

instance Functor (Sequence f g) where
    fmap f (Sequence m g) = Sequence m (fmap f g)

-- | A @Sequence@ with no values coming from an @Event@.
type SValue = Sequence Identity (Const Void)

-- | A @Sequence@ whose initial value contains no information.
--   This is much like an @Event@.
type SEvent = Sequence (Const ()) Identity

-- | A @Sequence@ with an initial value. This is much like a @Behavior@; it
--   contains enough information to produce a @Behavior@.
type SBehavior = Sequence Identity Identity

-- | Analogous to @stepper@.
(|>) :: t -> SEvent t -> SequenceM (SBehavior t)
(|>) = sEventToSBehavior

always :: Applicative f => t -> Sequence f g t
always x = Sequence (pure (pure x, pure never, never)) id

nothing :: Sequence (Const ()) (Const Void) t
nothing = Sequence (pure (Const (), pure never, never)) id

sequenceFirst :: Functor f => Sequence f g t -> MomentIO (f t)
sequenceFirst (Sequence m f) = do
    (first, _, _) <- m
    pure (f <$> first)

sequenceRest :: Functor g => Sequence f g t -> MomentIO (Event (g t))
sequenceRest (Sequence m f) = do
    (_, rest, _) <- m
    ev <- rest
    return ((fmap . fmap) f ev)

sequenceLag :: Functor g => Sequence f g t -> MomentIO (Event (g t))
sequenceLag (Sequence m f) = do
    (_, _, ev) <- m
    pure ((fmap . fmap) f ev)

-- | Take the lag of some behavior, giving a new behavior. They have the
--   same initial value, but the changes to the output happen immediately after
--   the changes to the input. Forcing the changes to the output does not
--   force the changes to the input.
lag :: (Functor f, Functor g) => Sequence f g t -> SequenceM (Sequence f g t)
lag sequence = SequenceM $ do
    first <- sequenceFirst sequence
    rest <- sequenceLag sequence
    (lag, fire) <- newEvent
    reactimate (fire <$> rest)
    pure (Sequence (pure (first, pure rest, lag)) id)

-- | Like @lag@, but stricter: forcing its changes *will* force the changes to
--   the thing from which it's derived. Useful if you want to get an event
--   immediately after another, whereas @lag@ is more useful in case you want
--   to do some recursion.
lag' :: (Functor f, Functor g) => Sequence f g t -> SequenceM (Sequence f g t)
lag' sequence = SequenceM $ do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    lag <- sequenceLag sequence
    rest' <- immediatelyAfter rest
    lag' <- immediatelyAfter lag
    pure (Sequence (pure (first, pure rest', lag')) id)

{-
sBehaviorLag :: SBehavior t -> SequenceM (SBehavior t)
sBehaviorLag sequence = SequenceM $ do
    first <- sequenceFirst sequence
    rest <- sequenceLag sequence
    (lag, fire) <- newEvent
    reactimate (fire <$> rest)
    pure (Sequence (pure (first, pure rest, lag)) id)
-}

-- | Analogous to @stepper@.
sEventToSBehavior :: t -> SEvent t -> SequenceM (SBehavior t)
sEventToSBehavior t sevent = SequenceM $ do
    (lag, fire) <- newEvent
    let theRest = do
            rest <- sequenceRest sevent
            reactimate (fire <$> rest)
            return rest
    pure (Sequence (pure (Identity t, theRest, lag)) id)

-- What are the implications of dumping the monadic part inside the derived
-- sequence? Every time you ask for the first or rest you get a new lag
-- event. Maybe that's ok?
{-
sEventToSBehavior :: t -> SEvent t -> SBehavior t
sEventToSBehavior t sevent = Sequence content id
  where
    content = do
        (lag, fire) <- newEvent
        let theRest = do
                rest <- sequenceRest sevent
                reactimate (fire <$> rest)
                return rest
        pure (Identity t, theRest, lag)
-}

-- | @SEvent@ is at least as big as @Event@.
eventToSEvent :: Event t -> SequenceM (SEvent t)
eventToSEvent ev = SequenceM $ do
    lag <- immediatelyAfter ev
    pure (Sequence (pure (Const (), pure (Identity <$> ev), Identity <$> lag)) id)
{-
-- Does this make a difference compared to the current one? Maybe we should
-- prefer it?
eventToSEvent :: Event t -> SEvent t
eventToSEvent ev = Sequence content id
  where
    content = do
        lag <- immediatelyAfter ev
        pure (Const (), pure (Identity <$> ev), Identity <$> lag)
-}

-- | An @Event@ can be recovered from any @SEvent@.
sEventToEvent :: SEvent t -> SequenceM (Event t)
sEventToEvent sevent = SequenceM $ (fmap . fmap) runIdentity (sequenceRest sevent)

-- | A @Behavior@ can be recovered from any @SBehavior@.
sBehaviorToBehavior :: SBehavior t -> SequenceM (Behavior t)
sBehaviorToBehavior sbehavior = SequenceM $ do
    initial :: Identity t <- sequenceFirst sbehavior
    rest :: Event (Identity t) <- sequenceRest sbehavior
    b <- stepper initial rest
    return (runIdentity <$> b)

-- | The changes to an @SBehavior@.
sBehaviorToSEvent :: SBehavior t -> SEvent t
sBehaviorToSEvent sbehavior = Sequence content id
  where
    content = do
        lag <- sequenceLag sbehavior
        pure (Const (), sequenceRest sbehavior, lag)

sequenceTrans
    :: forall f1 f2 g1 g2 t .
       (forall a . f1 a -> f2 a)
    -> (forall a . g1 a -> g2 a)
    -> Sequence f1 g1 t
    -> Sequence f2 g2 t
sequenceTrans transF transG (Sequence content h) = Sequence (alter <$> content) h
  where
    alter
        :: forall s .
           (f1 s, MomentIO (Event (g1 s)), Event (g1 s))
        -> (f2 s, MomentIO (Event (g2 s)), Event (g2 s))
    alter (s, rest, lag) = (transF s, (fmap . fmap) transG rest, fmap transG lag)

-- | @Sequence@s with homogeneous functor parameters are applicatives.
--   A more general applicative-style interface is given by
--
--     @
--       always :: f t -> Sequence f g t
--       (<%>) :: Sequence f1 g1 (s -> t) -> Sequence f2 g2 s -> Sequence f3 g3 t
--     @
--
--   Note that for (<%>) we rely on the Bundleable type class to determine
--   what the output parameters @f3@ and @g3@ shall be. This allows us to
--   combine SEvent and SBehavior, for example, to get an SEvent.
instance
    ( Applicative f
    , Applicative (BundleableIntermediate f f f f)
    , Bundleable f f f f
    , BundleableOutputF f f f f ~ f
    , BundleableOutputG f f f f ~ f
    ) => Applicative (Sequence f f)
  where
    pure = always
    (<*>) = (<%>)

-- | Combine two @Sequence@s.
--
--   This is general enough to express variants in which the resulting
--   sequence's event fires iff *one* of the input sequences fires, akin to
--   <@ from reactive-banana. It's obtain by choosing appropriate functions
--     (forall a . g1 s -> Maybe (g3 s))
--     (forall a . g2 s -> Maybe (g3 s))
--   These are run against every occurrence of the left (g1) and right (g2)
--   events (except for the immediate parts, which are irrelevant to this
--   discussion). Giving @Nothing@ means it should not fire, and giving
--   @Just@ means it should.
bundle
    :: forall f1 f2 f3 g1 g2 g3 h s t .
       ( Applicative f1
       , Applicative f2
       , Applicative f3
       , Applicative g1
       , Applicative g2
       , Applicative g3
       , Applicative h
       )
    => (forall a . f1 a -> f3 a)
    -> (forall a . f2 a -> f3 a)
    -> (forall a . g1 a -> Maybe (g3 a)) -- ^ Decide whether the left should
                                         --   be squelched, and if not, how
                                         --   to convert it to g3.
                                         --   Even if you give Nothing, the
                                         --   changes to the left are still
                                         --   observed, so that its latest
                                         --   value is used whenever the right
                                         --   fires.
    -> (forall a . g2 a -> Maybe (g3 a)) -- ^ Decide whether the right should
                                         --   be squelched, and if not, how
                                         --   to convert it to g3.
    -> (forall a . f1 a -> h a)
    -> (forall a . f2 a -> h a)
    -> (forall a . g1 a -> h a)
    -> (forall a . g2 a -> h a)
    -> (forall a . h a -> Either (f3 a) (g3 a))
    -> Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence f3 g3 (s, t)
bundle transF1
       transF2
       transG1
       transG2
       transF1H
       transF2H
       transG1H
       transG2H
       outH
       left
       right
   = Sequence content id
  where

    content :: MomentIO (f3 (s, t), MomentIO (Event (g3 (s, t))), Event (g3 (s, t)))
    content = do 
       firstl :: f1 s <- sequenceFirst left
       firstr :: f2 t <- sequenceFirst right
       lagl :: Event (g1 s) <- sequenceLag left
       lagr :: Event (g2 t) <- sequenceLag right
       lag <- bundleEvents (firstl, lagl) (firstr, lagr)
       let theRest :: MomentIO (Event (g3 (s, t)))
           theRest = do
               restl :: Event (g1 s) <- sequenceRest left
               restr :: Event (g2 t) <- sequenceRest right
               bundleEvents (firstl, restl) (firstr, restr)

       pure ( (,) <$> transF1 firstl <*> transF2 firstr
            , theRest
            , lag
            )

    bundleEvents :: (f1 s, Event (g1 s)) -> (f2 t, Event (g2 t)) -> MomentIO (Event (g3 (s, t)))
    bundleEvents (firstl, restl) (firstr, restr) = do
        bl :: Behavior (h s) <- stepper (transF1H firstl) (transG1H <$> restl)
        br :: Behavior (h t) <- stepper (transF2H firstr) (transG2H <$> restr)
        -- Now we create an event which merges s and t, handling the case in which
        -- they fire simultaneously by using the EitherBoth type.
        let evLDiscriminated :: Event (EitherBoth (g1 s) (g2 t))
            evLDiscriminated = OneLeft <$> restl
        let evRDiscriminated :: Event (EitherBoth (g1 s) (g2 t))
            evRDiscriminated = OneRight <$> restr
        let evLR :: Event (EitherBoth (g1 s) (g2 t))
            evLR = unionWith (\(OneLeft s) (OneRight t) -> Both s t)
                             (evLDiscriminated)
                             (evRDiscriminated)
        -- We throw in the behaviors for s and t, yielding an event which contains
        -- all the information we need in order to decide when to fire.
        let evLR' :: Event ((h s, h t), EitherBoth (g1 s) (g2 t))
            evLR' = (,) <$> ((,) <$> bl <*> br) <@> evLR
        let pick :: ((h s, h t), EitherBoth (g1 s) (g2 t)) -> Maybe (g3 (s, t))
            pick x = case x of
                -- If both fire, just give them.
                (_, Both s t) -> (\l r -> (,) <$> l <*> r) <$> transG1 s <*> transG2 t
                -- If only one fires, give the latest tuple, but only
                -- if the behavior yields a g3.
                ((_, t), OneLeft s) -> case outH t of
                    Left _ -> Nothing
                    Right t' -> (\l r -> (,) <$> l <*> r) <$> transG1 s <*> pure t'
                ((s, _), OneRight t) -> case outH s of
                    Left _ -> Nothing
                    Right s' -> (\l r -> (,) <$> l <*> r) <$> pure s' <*> transG2 t
        return (filterJust (pick <$> evLR'))

class Bundleable (f1 :: * -> *) (f2 :: * -> *) (g1 :: * -> *) (g2 :: * -> *) where
    type BundleableOutputF f1 f2 g1 g2 :: * -> *
    type BundleableOutputG f1 f2 g1 g2 :: * -> *
    type BundleableIntermediate f1 f2 g1 g2 :: * -> *
    bundleTransF1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 a -> BundleableOutputF f1 f2 g1 g2 a)
    bundleTransF2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f2 a -> BundleableOutputF f1 f2 g1 g2 a)
    bundleTransG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 a -> Maybe (BundleableOutputG f1 f2 g1 g2 a))
    bundleTransG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g2 a -> Maybe (BundleableOutputG f1 f2 g1 g2 a))
    bundleTransIntermediateF1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateF2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f2 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleTransIntermediateG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g2 a -> BundleableIntermediate f1 f2 g1 g2 a)
    bundleIntermediateOut
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . BundleableIntermediate f1 f2 g1 g2 a -> Either (BundleableOutputF f1 f2 g1 g2 a) (BundleableOutputG f1 f2 g1 g2 a))

-- | Proof that we can bundle SEvent and SBehavior.
instance Bundleable (Const ()) Identity Identity Identity where
    type BundleableOutputF (Const ()) Identity Identity Identity = Const ()
    type BundleableOutputG (Const ()) Identity Identity Identity = Identity
    type BundleableIntermediate (Const ()) Identity Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = Just
    bundleTransG2 _ = Just
    bundleTransIntermediateF1 _ = const Nothing
    bundleTransIntermediateF2 _ = Just . runIdentity
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SBehavior and SEvent.
instance Bundleable Identity (Const ()) Identity Identity where
    type BundleableOutputF Identity (Const ()) Identity Identity = Const ()
    type BundleableOutputG Identity (Const ()) Identity Identity = Identity
    type BundleableIntermediate Identity (Const ()) Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = Just
    bundleTransG2 _ = Just
    bundleTransIntermediateF1 _ = Just . runIdentity
    bundleTransIntermediateF2 _ = const Nothing
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SEvent and SEvent.
instance Bundleable (Const ()) (Const ()) Identity Identity where
    type BundleableOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type BundleableOutputG (Const ()) (Const ()) Identity Identity = Identity
    type BundleableIntermediate (Const ()) (Const ()) Identity Identity = Maybe
    bundleTransF1 _ = const (Const ())
    bundleTransF2 _ = const (Const ())
    bundleTransG1 _ = Just
    bundleTransG2 _ = Just
    bundleTransIntermediateF1 _ = const Nothing
    bundleTransIntermediateF2 _ = const Nothing
    bundleTransIntermediateG1 _ = Just . runIdentity
    bundleTransIntermediateG2 _ = Just . runIdentity
    bundleIntermediateOut _ = maybe (Left (Const ())) (Right . Identity)

-- | Proof that we can bundle SBehavior and SBehavior.
instance Bundleable Identity Identity Identity Identity where
    type BundleableOutputF Identity Identity Identity Identity = Identity
    type BundleableOutputG Identity Identity Identity Identity = Identity
    type BundleableIntermediate Identity Identity Identity Identity = Identity
    bundleTransF1 _ = id
    bundleTransF2 _ = id
    bundleTransG1 _ = Just
    bundleTransG2 _ = Just
    bundleTransIntermediateF1 _ = id
    bundleTransIntermediateF2 _ = id
    bundleTransIntermediateG1 _ = id
    bundleTransIntermediateG2 _ = id
    -- We choose Right to indicate that it's available.
    -- Has we chosen Left, the bundled behavior would never update.
    bundleIntermediateOut _ = Right

-- | Like @bundle@, but with the parameters filled in automatically via the
--   @Bundleable@ class.
bundle'
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) (s, t)
bundle' = bundle (bundleTransF1 proxy)
                 (bundleTransF2 proxy)
                 (bundleTransG1 proxy)
                 (bundleTransG2 proxy)
                 (bundleTransIntermediateF1 proxy)
                 (bundleTransIntermediateF2 proxy)
                 (bundleTransIntermediateG1 proxy)
                 (bundleTransIntermediateG2 proxy)
                 (bundleIntermediateOut proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

-- | Bundle but only fire when the right one fires. This is like <@ from
--   reactive-banana.
bundleRight'
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) (s, t)
bundleRight' = bundle (bundleTransF1 proxy)
                      (bundleTransF2 proxy)
                      (const Nothing)
                      (bundleTransG2 proxy)
                      (bundleTransIntermediateF1 proxy)
                      (bundleTransIntermediateF2 proxy)
                      (bundleTransIntermediateG1 proxy)
                      (bundleTransIntermediateG2 proxy)
                      (bundleIntermediateOut proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

-- | Bundle but only fire when the left one fires.
bundleLeft'
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 t
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) (s, t)
bundleLeft' = bundle (bundleTransF1 proxy)
                     (bundleTransF2 proxy)
                     (bundleTransG1 proxy)
                     (const Nothing)
                     (bundleTransIntermediateF1 proxy)
                     (bundleTransIntermediateF2 proxy)
                     (bundleTransIntermediateG1 proxy)
                     (bundleTransIntermediateG2 proxy)
                     (bundleIntermediateOut proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

infixl 4 <%>
(<%>)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
left <%> right = (uncurry ($)) <$> (bundle' left right)

infixl 4 <⌚>
-- | An operator that looks kindof like <*>, but with a watch instead of
--   a *.
(<⌚>)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
(<⌚>) = (<%>)

infixl 4 %>
-- | Notice how it points in the opposite direction from the one
--   indicated by the name @bundlLeft'@. That's because we want to be consistent
--   with reactive-banana's <@ operator, which fires only when the right-hand
--   term fires. This one fires only when the left-hand term fires, hence the
--   use of @bundleLeft'@.
(%>)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
left %> right = (uncurry ($)) <$> (bundleLeft' left right)

infixl 4 <%
(<%)
    :: forall f1 f2 g1 g2 s t .
       ( Applicative f1
       , Applicative f2
       , Applicative (BundleableOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (BundleableOutputG f1 f2 g1 g2)
       , Applicative (BundleableIntermediate f1 f2 g1 g2)
       , Bundleable f1 f2 g1 g2
       )
    => Sequence f1 g1 (s -> t)
    -> Sequence f2 g2 s
    -> Sequence (BundleableOutputF f1 f2 g1 g2) (BundleableOutputG f1 f2 g1 g2) t
left <% right = (uncurry ($)) <$> (bundleRight' left right)

-- | Define an SBehavior in terms of its own Behavior
fixSBehavior
    :: forall t .
       ( SBehavior t -> SequenceM (SBehavior t) )
    -> SequenceM (SBehavior t)
fixSBehavior makeIt = mdo
    sbehavior <- makeIt lagged
    lagged <- lag sbehavior
    pure sbehavior

-- | Sort of like @<|>@, as @<%>@ is for @<*>@. This combinator gives the
--   @Sequence@ of the *latest* values for both input @Sequence@s, subject
--   to disambiguator functions for simultaneous occurrences.
--   Contrast with @<%>@, which downgrades input @Sequence@s to their
--   greatest lower bound; this one upgrades them to the least upper bound!
--   This is intuitive: a behavior always has a latest value, so if we union
--   it with an event, which may not have a latest value, we still have a
--   latest value.
sequenceUnion
    :: forall f1 f2 f3 g1 g2 g3 s .
       ( Applicative f1
       , Applicative f2
       , Applicative f3
       , Applicative g1
       , Applicative g2
       , Applicative g3
       )
    => (s -> s -> s)
    -> ((s -> s -> s) -> f1 s -> f2 s -> f3 s)
    -> ((s -> s -> s) -> g3 s -> g3 s -> g3 s)
    -> (forall a . g1 a -> g3 a)
    -> (forall a . g2 a -> g3 a)
    -> Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence f3 g3 s
sequenceUnion disambiguator disambiguatorF disambiguatorG transG1 transG2 left right = Sequence content id
  where
    content = do
        firstl :: f1 s <- sequenceFirst left
        firstr :: f2 s <- sequenceFirst right
        lagl :: Event (g1 s) <- sequenceLag left
        lagr :: Event (g2 s) <- sequenceLag right
        let theRest = do
                restl :: Event (g1 s) <- sequenceRest left
                restr :: Event (g2 s) <- sequenceRest right
                return (unionWith (disambiguatorG disambiguator) (transG1 <$> restl) (transG2 <$> restr))
        pure ( disambiguatorF disambiguator firstl firstr
             , theRest
             , unionWith (disambiguatorG disambiguator) (transG1 <$> lagl) (transG2 <$> lagr)
             )

class
    ( Applicative f1
    , Applicative f2
    , Applicative g1
    , Applicative g2
    , Applicative (UnionOutputF f1 f2 g1 g2)
    , Applicative (UnionOutputG f1 f2 g1 g2)
    ) => Unionable f1 f2 g1 g2
  where
    type UnionOutputF f1 f2 g1 g2 :: * -> *
    type UnionOutputG f1 f2 g1 g2 :: * -> *
    unionDisambiguatorF
        :: Proxy '(f1, f2, g1, g2)
        -> (s -> s -> s)
        -> f1 s
        -> f2 s
        -> UnionOutputF f1 f2 g1 g2 s
    unionDisambiguatorG
        :: Proxy '(f1, f2, g1, g2)
        -> (s -> s -> s)
        -> UnionOutputG f1 f2 g1 g2 s
        -> UnionOutputG f1 f2 g1 g2 s
        -> UnionOutputG f1 f2 g1 g2 s
    unionTransG1
        :: Proxy '(f1, f2, g1, g2)
        -> (forall s . g1 s -> UnionOutputG f1 f2 g1 g2 s)
    unionTransG2
        :: Proxy '(f1, f2, g1, g2)
        -> (forall s . g2 s -> UnionOutputG f1 f2 g1 g2 s)

instance Unionable (Const ()) Identity Identity Identity where
    type UnionOutputF (Const ()) Identity Identity Identity = Identity
    type UnionOutputG (Const ()) Identity Identity Identity = Identity
    unionDisambiguatorF _ _ _ = id
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable Identity (Const ()) Identity Identity where
    type UnionOutputF Identity (Const ()) Identity Identity = Identity
    type UnionOutputG Identity (Const ()) Identity Identity = Identity
    unionDisambiguatorF _ _ x = const x
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable Identity Identity Identity Identity where
    type UnionOutputF Identity Identity Identity Identity = Identity
    type UnionOutputG Identity Identity Identity Identity = Identity
    unionDisambiguatorF _ f l r = f <$> l <*> r
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

instance Unionable (Const ()) (Const ()) Identity Identity where
    type UnionOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type UnionOutputG (Const ()) (Const ()) Identity Identity = Identity
    unionDisambiguatorF _ _ _ _ = Const ()
    unionDisambiguatorG _ f l r = f <$> l <*> r
    unionTransG1 _ = id
    unionTransG2 _ = id

type family UnionsTo s1 s2 :: * -> * where
    UnionsTo (Sequence f1 g1) (Sequence f2 g2) = Sequence (UnionOutputF f1 f2 g1 g2)
                                                          (UnionOutputG f1 f2 g1 g2)

-- | Like @sequenceUnion@, but with some parameters filled in automatically
--   via @Unionable@.
sequenceUnion'
    :: forall f1 f2 g1 g2 s .
       ( Unionable f1 f2 g1 g2
       , Applicative f1
       , Applicative f2
       , Applicative (UnionOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (UnionOutputG f1 f2 g1 g2)
       )
    => (s -> s -> s)
    -> Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence (UnionOutputF f1 f2 g1 g2) (UnionOutputG f1 f2 g1 g2) s
sequenceUnion' disambiguator = sequenceUnion disambiguator
                                             (unionDisambiguatorF proxy)
                                             (unionDisambiguatorG proxy)
                                             (unionTransG1 proxy)
                                             (unionTransG2 proxy)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

infixl 4 <||>
(<||>)
    :: forall f1 f2 g1 g2 s .
       ( Unionable f1 f2 g1 g2
       , Applicative f1
       , Applicative f2
       , Applicative (UnionOutputF f1 f2 g1 g2)
       , Applicative g1
       , Applicative g2
       , Applicative (UnionOutputG f1 f2 g1 g2)
       , Semigroup s
       )
    => Sequence f1 g1 s
    -> Sequence f2 g2 s
    -> Sequence (UnionOutputF f1 f2 g1 g2) (UnionOutputG f1 f2 g1 g2) s
(<||>) = sequenceUnion' (<>)

-- | Commute a Sequence with MomentIO. This will force the first and rest of
--   the sequence.
sequenceCommute
    :: forall f g t .
       ( Functor f, Functor g )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g (MomentIO t)
    -> SequenceM (Sequence f g t)
sequenceCommute commuteF commuteG sequence = SequenceM $ do
    first :: f (MomentIO t) <- sequenceFirst sequence
    rest :: Event (g (MomentIO t)) <- sequenceRest sequence
    executedFirst <- commuteF first
    executedRest <- execute (commuteG <$> rest)
    (lag, fire) <- newEvent
    reactimate (fire <$> executedRest)
    pure (Sequence (pure (executedFirst, pure executedRest, lag)) id)

-- | A highly general variant of @switchE :: Event (Event t) -> Event t@.
--
--   TODO understand and explain why the output has to be in MomentIO.
switchSequence
    :: forall f1 f2 f3 g1 g2 g3 t .
       ( Functor f1
       , Functor f2
       , Functor f3
       , Functor g1
       , Functor g2
       , Functor g3
       )
    => (forall t . f1 (MomentIO t) -> MomentIO (f1 t))
    -> (forall t . g1 (MomentIO t) -> MomentIO (g1 t))
    -> (forall t . f1 (f2 t) -> f3 t) -- ^ The immediate part of the output
                                      --   sequence is computed through the
                                      --   immediate part functors of both.
                                      --   If one of f1, f2 is Const (), for
                                      --   instance, then f3 must be Const(),
                                      --   indicating that if either sequence
                                      --   has no immediate part, then the
                                      --   resulting sequence has no immediate
                                      --   part.
    -> (forall t . f1 (Event (g2 t)) -> Event (g3 t))
    -> (forall t . Event (g1 (f2 t)) -> Event (g3 t))
    -> (forall t . g1 (Event (g2 t)) -> Event (g3 t))
    -> (g3 t -> g3 t -> g3 t)
    -> Sequence f1 g1 (Sequence f2 g2 t)
    -> SequenceM (Sequence f3 g3 t)
switchSequence commuteF1
               commuteG1
               joinF1F2
               eventF1
               eventG1F2
               eventG1
               disambiguatorG3
               sequence
    = SequenceM $ do 
          -- To get the first part, we take the first part of the outer
          -- @Sequence@, which is itself a @Sequenc@e, but guarded by @f1@.
          first :: f1 (Sequence f2 g2 t) <- sequenceFirst sequence
          -- We then take the first part of the inner @Sequence@, to get a
          -- @MomentIO (f2 t)@, and commute it with @f1@ to bring the @MomentIO@
          -- out front.
          -- So for example, if @f1 ~ Const ()@ because the outer @Sequence@ is
          -- an @SEvent@, we'll just have @Const ()@ here; there is no initial
          -- value of the switched @Sequence@.
          first' :: f1 (f2 t) <- commuteF1 (sequenceFirst <$> first)

          -- TBD does it matter whether we do the following computation here,
          -- or wrap it up in a MomentIO are throw it into the second place of
          -- the resulting sequence?

          -- The remaining @Sequence@s in the outer @Sequence@.
          rest :: Event (g1 (Sequence f2 g2 t)) <- sequenceRest sequence
          -- The remaining elements of the first part of the outer
          -- @Sequence@.
          firstRest :: f1 (Event (g2 t))
              <- commuteF1 (sequenceRest <$> first)
          -- The first (immediate) elements of the remaining sequences.
          restFirst :: Event (g1 (f2 t))
              <- execute ((commuteG1 . fmap sequenceFirst) <$> rest)
          -- The remaining elements of the remaining sequences.
          restRest :: Event (g1 (Event (g2 t)))
              <- execute ((commuteG1 . fmap sequenceRest) <$> rest)
          let switchedRest :: Event (g3 t)
              switchedRest = switchE (eventG1 <$> restRest)
          -- We want to use firstRest until restFirst has fired at least
          -- once, at which point we union restFirst with restRest.
          restHasFired :: Behavior Bool <- stepper False (const True <$> restFirst)
          let remaining :: Event (g3 t)
              remaining = unionWith disambiguatorG3
                                    (eventG1F2 restFirst)
                                    (switchedRest)
          let ev = unionWith disambiguatorG3
                             (filterApply ((const . not) <$> restHasFired) (eventF1 firstRest))
                             (filterApply (const <$> restHasFired) remaining)
          (lag, fire) <- newEvent
          reactimate (fire <$> ev)

          return (Sequence (pure (joinF1F2 first', pure ev, lag)) id)

class Switchable f1 f2 g1 g2 where
    type SwitchableOutputF f1 f2 g1 g2 :: * -> *
    type SwitchableOutputG f1 f2 g1 g2 :: * -> *
    switchableCommuteF
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 (MomentIO a) -> MomentIO (f1 a))
    switchableCommuteG
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 (MomentIO a) -> MomentIO (g1 a))
    switchableJoinF
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 (f2 a) -> SwitchableOutputF f1 f2 g1 g2 a)
    switchableJoinEventF
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . f1 (Event (g2 a)) -> Event (SwitchableOutputG f1 f2 g1 g2 a))
    switchableJoinEventFG
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . Event (g1 (f2 a)) -> Event (SwitchableOutputG f1 f2 g1 g2 a))
    switchableJoinEventG
        :: Proxy '(f1, f2, g1, g2)
        -> (forall a . g1 (Event (g2 a)) -> Event (SwitchableOutputG f1 f2 g1 g2 a))
    switchableDisambiguator
        :: Proxy '(f1, f2, g1, g2)
        -> ( forall t .
                (t -> t -> t)
             -> SwitchableOutputG f1 f2 g1 g2 t
             -> SwitchableOutputG f1 f2 g1 g2 t
             -> SwitchableOutputG f1 f2 g1 g2 t
           )

-- | An event of behaviors gives no initial value.
instance Switchable (Const ()) Identity Identity Identity where
    type SwitchableOutputF (Const ()) Identity Identity Identity = (Const ())
    type SwitchableOutputG (Const ()) Identity Identity Identity = Identity
    switchableCommuteF _ = const (pure (Const ()))
    switchableCommuteG _ = fmap Identity . runIdentity
    switchableJoinF _ = const (Const ())
    switchableJoinEventF _ = const never
    switchableJoinEventFG _ = fmap runIdentity
    switchableJoinEventG _ = runIdentity
    switchableDisambiguator _ disambiguator l r = disambiguator <$> l <*> r

-- | A behavior of events gives no initial value.
instance Switchable Identity (Const ()) Identity Identity where
    type SwitchableOutputF Identity (Const ()) Identity Identity = Const ()
    type SwitchableOutputG Identity (Const ()) Identity Identity = Identity
    switchableCommuteF _ = fmap Identity . runIdentity
    switchableCommuteG _ = fmap Identity . runIdentity
    switchableJoinF _ = const (Const ())
    switchableJoinEventF _ = runIdentity
    -- forall a . Identity (Const () t) -> Identity t
    -- is not possible, so we give never.
    switchableJoinEventFG _ = const never
    switchableJoinEventG _ = runIdentity
    switchableDisambiguator _ disambiguator l r = disambiguator <$> l <*> r

instance Switchable (Const ()) (Const ()) Identity Identity where
    type SwitchableOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type SwitchableOutputG (Const ()) (Const ()) Identity Identity = Identity
    switchableCommuteF _ = const (pure (Const ()))
    switchableCommuteG _ = fmap Identity . runIdentity
    switchableJoinF _ = const (Const ())
    switchableJoinEventF _ = const never
    switchableJoinEventFG _ = const never
    switchableJoinEventG _ = runIdentity
    switchableDisambiguator _ disambiguator l r = disambiguator <$> l <*> r

instance Switchable Identity Identity Identity Identity where
    type SwitchableOutputF Identity Identity Identity Identity = Identity
    type SwitchableOutputG Identity Identity Identity Identity = Identity
    switchableCommuteF _ = fmap Identity . runIdentity
    switchableCommuteG _ = fmap Identity . runIdentity
    switchableJoinF _ = runIdentity
    switchableJoinEventF _ = runIdentity
    switchableJoinEventFG _ = fmap runIdentity
    switchableJoinEventG _ = runIdentity
    switchableDisambiguator _ disambiguator l r = disambiguator <$> l <*> r

type family SwitchesTo s1 :: * where
    SwitchesTo (Sequence f1 g1 (Sequence f2 g2 s)) = Sequence (SwitchableOutputF f1 f2 g1 g2)
                                                              (SwitchableOutputG f1 f2 g1 g2)
                                                              s

switchSequence'
    :: forall f1 f2 g1 g2 t .
       ( Functor f1
       , Functor f2
       , Functor (SwitchableOutputF f1 f2 g1 g2)
       , Functor g1
       , Functor g2
       , Functor (SwitchableOutputG f1 f2 g1 g2)
       , Switchable f1 f2 g1 g2
       )
    => (t -> t -> t)
    -> Sequence f1 g1 (Sequence f2 g2 t)
    -> SequenceM (Sequence (SwitchableOutputF f1 f2 g1 g2) (SwitchableOutputG f1 f2 g1 g2) t)
switchSequence' disambiguator = switchSequence (switchableCommuteF proxy)
                                               (switchableCommuteG proxy)
                                               (switchableJoinF proxy)
                                               (switchableJoinEventF proxy)
                                               (switchableJoinEventFG proxy)
                                               (switchableJoinEventG proxy)
                                               (switchableDisambiguator proxy disambiguator)
  where
    proxy :: Proxy '(f1, f2, g1, g2)
    proxy = Proxy

-- | Just a new name for @switchSequence'@
switch
    :: forall f1 f2 g1 g2 t .
       ( Functor f1
       , Functor f2
       , Functor (SwitchableOutputF f1 f2 g1 g2)
       , Functor g1
       , Functor g2
       , Functor (SwitchableOutputG f1 f2 g1 g2)
       , Switchable f1 f2 g1 g2
       )
    => (t -> t -> t)
    -> Sequence f1 g1 (Sequence f2 g2 t)
    -> SequenceM (Sequence (SwitchableOutputF f1 f2 g1 g2) (SwitchableOutputG f1 f2 g1 g2) t)
switch = switchSequence'

sequenceReactimate
    :: forall f g .
       ( Functor f, Functor g )
    => (f (IO ()) -> IO ())
    -> (g (IO ()) -> IO ())
    -> Sequence f g (IO ())
    -> SequenceM ()
sequenceReactimate elimF elimG sequence = SequenceM $ do
    first :: f (IO ()) <- sequenceFirst sequence
    rest :: Event (g (IO ())) <- sequenceRest sequence
    liftIO (elimF first)
    reactimate (elimG <$> rest)
    return ()
