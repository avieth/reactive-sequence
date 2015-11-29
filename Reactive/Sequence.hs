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
      Sequence(..)
    , SValue
    , SEvent
    , SBehavior
    , sequenceTrans
    , (|>)
    , always
    , always'
    , never
    , never'
    , lag
    , lag'
    , sEventToSBehavior
    , sBehaviorToSEvent
    , sequenceTrim
    , eventToSEvent
    , sEventToEvent
    , sBehaviorToBehavior
    , fixSBehavior
    , bundle
    , Bundleable
    , BundlesTo
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
    , switchSequence
    , Switchable
    , SwitchesTo
    , switchSequence'
    , switch
    , sequenceFilter
    , sequenceReactimate
    , sequenceExecute
    , sequenceCommute
    , Commutable
    , sequenceCommutor
    , immediatelyAfter
    ) where

import Control.Monad
import Control.Monad.Fix
import Control.Monad.IO.Class
import Data.Void
import Data.Proxy
import Data.Functor.Identity
import Data.EitherBoth
import Data.Semigroup
import Reactive.Banana.Combinators hiding (never)
import qualified Reactive.Banana.Combinators as Banana
import Reactive.Banana.Frameworks
import Data.IORef
import System.IO.Unsafe

immediatelyAfter :: Event t -> MomentIO (Event t)
immediatelyAfter ev = do
    (ev', fire) <- newEvent
    reactimate (fire <$> ev)
    return ev'

-- | A cached MomentIO computation.
data SequenceM t where
    SequenceMLift :: IORef (Either t (MomentIO t)) -> SequenceM t
    SequenceMPure :: t -> SequenceM t
    SequenceMFmap :: (s -> t) -> SequenceM s -> SequenceM t
    SequenceMBind :: SequenceM s -> (s -> SequenceM t) ->  SequenceM t
    SequenceMFix :: (t -> SequenceM t) -> SequenceM t

-- | Make a MomentIO computation cached, so that it will run at most once, and
--   subsequent calls will just obtain the value without side-effects.
sequenceM :: MomentIO t -> SequenceM t
sequenceM m = unsafePerformIO $ do
    ref <- newIORef (Right m)
    pure (SequenceMLift ref)

-- | Run a SequenceM in MomentIO. This is not thread-safe.
runSequenceM :: forall t . SequenceM t -> MomentIO t
runSequenceM term = case term of
    SequenceMLift ref -> do
        -- This should be fine. I don't believe Sequences will be forced
        -- concurrently... there's no reason for it. Obviously could be wrong
        -- though.
        x <- liftIO $ readIORef ref
        case x of
            Left val -> pure val
            Right uncomputed -> do
                val <- uncomputed
                liftIO $ writeIORef ref (Left val)
                pure val
    SequenceMPure t -> pure t
    SequenceMFmap f m -> fmap f (runSequenceM m)
    SequenceMBind m k -> runSequenceM m >>= runSequenceM . k
    SequenceMFix knot -> let untied :: t -> MomentIO t
                             untied = fmap runSequenceM knot
                         in  mfix untied

instance Functor SequenceM where
    fmap = SequenceMFmap

instance Applicative SequenceM where
    pure = SequenceMPure
    (<*>) = ap

instance Monad SequenceM where
    return = pure
    (>>=) = SequenceMBind

instance MonadFix SequenceM where
    mfix = SequenceMFix

instance MonadIO SequenceM where
    liftIO = sequenceM . liftIO

instance MonadMoment SequenceM where
    liftMoment = sequenceM . liftMoment

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
    -- | Holds, in order, the initial value, the remaining values, and
    --   the remaining values lagged one frame, to allow for recursive
    --   possibilities.
    --   The whole thing is in a SequenceM because we need that in order to
    --   produce the lagged event. The internal SequenceM is there so that
    --   we don't necessarily have to force the remaining events just to
    --   work with a sequence's internal parts.
    Sequence
        :: SequenceM (f t, SequenceM (Event (g t)), Event (g t))
        -> Sequence f g t

instance (Functor f, Functor g) => Functor (Sequence f g) where
    fmap f (Sequence m) = Sequence (fmap f' m)
      where
        f' (immediate, remaining, lagged) = ( (fmap) f immediate
                                            , (fmap . fmap . fmap) f remaining
                                            , (fmap . fmap) f lagged)

-- | A @Sequence@ with no values coming from an @Event@.
type SValue = Sequence Identity (Const Void)

-- | A @Sequence@ whose initial value contains no information.
--   This is much like an @Event@.
type SEvent = Sequence (Const ()) Identity

-- | A @Sequence@ with an initial value. This is much like a @Behavior@; it
--   contains enough information to produce a @Behavior@.
type SBehavior = Sequence Identity Identity

sequenceFirst :: forall f g t . Functor f => Sequence f g t -> MomentIO (f t)
sequenceFirst (Sequence m)  = do
    (first, _, _) <- runSequenceM m
    pure first

sequenceRest :: forall f g t . Functor g => Sequence f g t -> MomentIO (Event (g t))
sequenceRest (Sequence m) = do
    (_, rest, _) <- runSequenceM m
    runSequenceM rest

sequenceLag :: forall f g t . Functor g => Sequence f g t -> MomentIO (Event (g t))
sequenceLag (Sequence m) = do
    (_, _, ev) <- runSequenceM m
    pure ev

always :: forall t . t -> SBehavior t
always = always'

always' :: forall t g . t -> Sequence Identity g t
always' x = Sequence (pure (pure x, pure Banana.never, Banana.never))

never :: forall t . SEvent t
never = never'

never' :: forall t g . Sequence (Const ()) g t
never' = Sequence (pure (Const (), pure Banana.never, Banana.never))

-- | Analogous to @stepper@.
sEventToSBehavior :: forall t . t -> SEvent t -> SBehavior t
sEventToSBehavior t sevent = Sequence . sequenceM $ do
    (lagged, fire) <- newEvent
    let theRest :: SequenceM (Event (Identity t))
        theRest = sequenceM $ do
            rest <- sequenceRest sevent
            reactimate (fire <$> rest)
            return rest
    pure (Identity t, theRest, lagged)

-- | Alias for @sEventToSBehavior@
(|>) :: t -> SEvent t -> SBehavior t
(|>) = sEventToSBehavior

-- | Take the lag of some behavior, giving a new behavior. They have the
--   same initial value, but the changes to the output happen immediately after
--   the changes to the input. Forcing the changes to the output does not
--   force the changes to the input.
--
--   NB this does not touch the rest of the sequence, only its immediate part
--   and its lag. If the rest is never forced, then the rest of this
--   will never fire.
lag :: forall f g t . (Functor f, Functor g) => Sequence f g t -> Sequence f g t
lag sequence = Sequence . sequenceM $ do
    first <- sequenceFirst sequence
    rest <- sequenceLag sequence
    (lag, fire) <- newEvent
    reactimate (fire <$> rest)
    pure (first, pure rest, lag)

-- | Like @lag@, but stricter: forcing its changes *will* force the changes to
--   the thing from which it's derived. Useful if you want to get an event
--   immediately after another, whereas @lag@ is more useful in case you want
--   to do some recursion.
lag' :: forall f g t . (Functor f, Functor g) => Sequence f g t -> Sequence f g t
lag' sequence = Sequence . sequenceM $ do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    lag <- sequenceLag sequence
    rest' <- immediatelyAfter rest
    lag' <- immediatelyAfter lag
    pure (first, pure rest', lag')


-- | @SEvent@ is at least as big as @Event@.
eventToSEvent :: Event t -> SEvent t
eventToSEvent ev = Sequence . sequenceM $ do
    lagged <- immediatelyAfter ev
    pure (Const (), pure (Identity <$> ev), Identity <$> lagged)

-- | An @Event@ can be recovered from any @SEvent@.
sEventToEvent :: SEvent t -> MomentIO (Event t)
sEventToEvent sevent = (fmap . fmap) runIdentity (sequenceRest sevent)

-- | A @Behavior@ can be recovered from any @SBehavior@.
sBehaviorToBehavior :: SBehavior t -> MomentIO (Behavior t)
sBehaviorToBehavior sbehavior = do
    initial :: Identity t <- sequenceFirst sbehavior
    rest :: Event (Identity t) <- sequenceRest sbehavior
    b <- stepper initial rest
    return (runIdentity <$> b)

-- | Drop the first value to obtain only the changes.
sBehaviorToSEvent :: SBehavior t -> SEvent t
sBehaviorToSEvent sbehavior = Sequence . sequenceM $ do
    lagged <- sequenceLag sbehavior
    rest <- sequenceRest sbehavior
    pure (Const (), pure rest, lagged)

-- | Does not force the first of the input sequence, just drops it.
sequenceTrim :: (Functor g) => Sequence f g t -> Sequence (Const ()) g t
sequenceTrim sbehavior = Sequence . sequenceM $ do
    (lagged, fire) <- newEvent
    let theRest = sequenceM $ do
            rest <- sequenceRest sbehavior
            reactimate (fire <$> rest)
            pure rest
    pure (Const (), theRest, lagged)

sequenceTrans
    :: forall f1 f2 g1 g2 t .
       (forall a . f1 a -> f2 a)
    -> (forall a . g1 a -> g2 a)
    -> Sequence f1 g1 t
    -> Sequence f2 g2 t
sequenceTrans transF transG (Sequence content) = Sequence (alter <$> content)
  where
    alter
        :: forall s .
           (f1 s, SequenceM (Event (g1 s)), Event (g1 s))
        -> (f2 s, SequenceM (Event (g2 s)), Event (g2 s))
    alter (s, rest, lag) = (transF s, (fmap . fmap) transG rest, fmap transG lag)

instance Applicative SBehavior where
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
   = Sequence . sequenceM $ do
         firstl :: f1 s <- sequenceFirst left
         firstr :: f2 t <- sequenceFirst right
         lagl :: Event (g1 s) <- sequenceLag left
         lagr :: Event (g2 t) <- sequenceLag right
         lag <- bundleEvents (firstl, lagl) (firstr, lagr)
         let theRest :: SequenceM (Event (g3 (s, t)))
             theRest = sequenceM $ do
                 restl :: Event (g1 s) <- sequenceRest left
                 restr :: Event (g2 t) <- sequenceRest right
                 bundleEvents (firstl, restl) (firstr, restr)

         pure ( (,) <$> transF1 firstl <*> transF2 firstr
              , theRest
              , lag
              )

  where

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

class
    ( Applicative f1
    , Applicative f2
    , Applicative g1
    , Applicative g2
    , Applicative (BundleableOutputF f1 f2 g1 g2)
    , Applicative (BundleableOutputG f1 f2 g1 g2)
    , Applicative (BundleableIntermediate f1 f2 g1 g2)
    ) => Bundleable (f1 :: * -> *) (f2 :: * -> *) (g1 :: * -> *) (g2 :: * -> *)
  where
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

type family BundlesTo s1 s2 :: * -> * where
    BundlesTo (Sequence f1 g1) (Sequence f2 g2) = Sequence (BundleableOutputF f1 f2 g1 g2)
                                                           (BundleableOutputG f1 f2 g1 g2)
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

-- | Define an SBehavior in terms of itself.
fixSBehavior :: forall t . (SBehavior t -> SBehavior t) -> SBehavior t
fixSBehavior makeIt = Sequence . sequenceM $ mdo
    let sbehavior = makeIt laggedSelf
    -- Important that we use lag not lag'.
    let laggedSelf = lag sbehavior
    first <- sequenceFirst sbehavior
    lagged <- sequenceLag sbehavior
    rest <- sequenceRest sbehavior
    pure (first, pure rest, lagged)

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
sequenceUnion disambiguator
              disambiguatorF
              disambiguatorG
              transG1
              transG2
              left
              right
    = Sequence . sequenceM $ do
          firstl :: f1 s <- sequenceFirst left
          firstr :: f2 s <- sequenceFirst right
          lagl :: Event (g1 s) <- sequenceLag left
          lagr :: Event (g2 s) <- sequenceLag right
          let theRest = sequenceM $ do
                  restl :: Event (g1 s) <- sequenceRest left
                  restr :: Event (g2 s) <- sequenceRest right
                  pure (unionWith (disambiguatorG disambiguator)
                                  (transG1 <$> restl)
                                  (transG2 <$> restr)
                       )
          pure ( disambiguatorF disambiguator firstl firstr
               , theRest
               , unionWith (disambiguatorG disambiguator)
                           (transG1 <$> lagl)
                           (transG2 <$> lagr)
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
    -> Sequence f3 g3 t
switchSequence commuteF1
               commuteG1
               joinF1F2
               eventF1
               eventG1F2
               eventG1
               disambiguatorG3
               sequence
    = Sequence . sequenceM $ do 
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

          (lag, fire) <- newEvent

          let theRest = sequenceM $ do
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
                  reactimate (fire <$> ev)
                  pure ev

          pure (joinF1F2 first', theRest, lag)

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
    switchableJoinEventF _ = const Banana.never
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
    switchableJoinEventFG _ = const Banana.never
    switchableJoinEventG _ = runIdentity
    switchableDisambiguator _ disambiguator l r = disambiguator <$> l <*> r

instance Switchable (Const ()) (Const ()) Identity Identity where
    type SwitchableOutputF (Const ()) (Const ()) Identity Identity = Const ()
    type SwitchableOutputG (Const ()) (Const ()) Identity Identity = Identity
    switchableCommuteF _ = const (pure (Const ()))
    switchableCommuteG _ = fmap Identity . runIdentity
    switchableJoinF _ = const (Const ())
    switchableJoinEventF _ = const Banana.never
    switchableJoinEventFG _ = const Banana.never
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
    -> Sequence (SwitchableOutputF f1 f2 g1 g2) (SwitchableOutputG f1 f2 g1 g2) t
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
    -> Sequence (SwitchableOutputF f1 f2 g1 g2) (SwitchableOutputG f1 f2 g1 g2) t
switch = switchSequence'

-- | Filter a sequence by identifying those things which you would like to
--   keep. Must also give a way to commute @g@ with @Maybe@.
sequenceFilter
    :: forall g s t .
       ( Functor g )
    => (forall a . g (Maybe a) -> Maybe (g a))
    -> (s -> Maybe t)
    -> Sequence (Const ()) g s
    -> Sequence (Const ()) g t
sequenceFilter commuteG pick sequence = Sequence . sequenceM $ do
    lagged <- sequenceLag sequence
    let theRest = sequenceM $ do
            rest <- sequenceRest sequence
            pure (filterJust (fmap (commuteG . fmap pick) rest))
    pure (Const (), theRest, filterJust (fmap (commuteG . fmap pick) lagged))

sequenceReactimate
    :: forall f g .
       ( Functor f, Functor g )
    => (forall s . f (IO s) -> IO (f s))
    -> (forall s . g (IO s) -> IO (g s))
    -> (f () -> ())
    -> (g () -> ())
    -> Sequence f g (IO ())
    -> MomentIO ()
sequenceReactimate commuteF commuteG elimF elimG sequence = do
    first :: f (IO ()) <- sequenceFirst sequence
    rest :: Event (g (IO ())) <- sequenceRest sequence
    liftIO (elimF <$> commuteF first)
    reactimate (fmap elimG . commuteG <$> rest)
    return ()

-- | Execute a Sequence, analogous to reactive-banana execute.
--   This will force the first and rest of the sequence.
sequenceExecute
    :: forall f g t .
       ( Functor f, Functor g )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g (MomentIO t)
    -> MomentIO (Sequence f g t)
sequenceExecute commuteF commuteG sequence = do
    first <- sequenceFirst sequence
    rest <- sequenceRest sequence
    executedFirst <- commuteF first
    executedRest <- execute (commuteG <$> rest)
    (lag, fire) <- newEvent
    reactimate (fire <$> executedRest)
    pure (Sequence (pure (executedFirst, pure executedRest, lag)))

-- | Like sequenceExecute, except that the execution is performed on-demand.
--   It does precisely the same thing as sequenceExecute, but the effects are
--   realized only when the sequence is forced.
sequenceCommute
    :: forall f g t .
       ( Functor f, Functor g )
    => (forall s . f (MomentIO s) -> MomentIO (f s))
    -> (forall s . g (MomentIO s) -> MomentIO (g s))
    -> Sequence f g (MomentIO t)
    -> Sequence f g t
sequenceCommute commuteF commuteG sequence = Sequence . sequenceM $ do
    first :: f (MomentIO t) <- sequenceFirst sequence
    rest :: Event (g (MomentIO t)) <- sequenceRest sequence
    executedFirst <- commuteF first
    executedRest <- execute (commuteG <$> rest)
    (lag, fire) <- newEvent
    reactimate (fire <$> executedRest)
    pure (executedFirst, pure executedRest, lag)

class Commutable f where
    sequenceCommutor :: f (MomentIO s) -> MomentIO (f s)

instance Commutable Identity where
    sequenceCommutor = fmap Identity . runIdentity

instance Commutable (Const ()) where
    sequenceCommutor = const (pure (Const ()))
