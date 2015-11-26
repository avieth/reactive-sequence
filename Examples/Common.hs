{-|
Module      : Examples.Common
Description : A module with some useful functions for all examples.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}

module Examples.Common where

import Control.Monad.IO.Class (liftIO)
import Control.Concurrent (threadDelay, forkIO, killThread)
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence

timedEvent :: Int -> IO t -> MomentIO (SEvent t, IO ())
timedEvent timeout mk = do
    (ev, fire) <- newEvent
    threadId <- liftIO $ forkIO (fireRepeatedly timeout mk fire)
    sevent <- runSequenceM (eventToSEvent ev)
    pure (sevent, killThread threadId)
  where
    fireRepeatedly timeout mk fire = do
        threadDelay timeout
        x <- mk
        fire x
        fireRepeatedly timeout mk fire
