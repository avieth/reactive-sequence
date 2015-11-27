{-|
Module      : Examples.Union
Description : Example of sequence unioning.
Copyright   : (c) Alexander Vieth, 2015
Licence     : BSD3
Maintainer  : aovieth@gmail.com
Stability   : experimental
Portability : non-portable (GHC only)
-}

{-# LANGUAGE AutoDeriveTypeable #-}
{-# LANGUAGE ScopedTypeVariables #-}

import Data.IORef
import Data.Functor.Identity
import Reactive.Banana.Combinators
import Reactive.Banana.Frameworks
import Reactive.Sequence
import Examples.Common

main = do

    killThreads <- newIORef []

    let networkDescription = do

            -- Every 1/2 of a second, fire event 1
            (ev1, kill1) <- timedEvent (round (1/2 * 1000000)) (pure "HALF")
            -- Every 1 second, fire event 2
            (ev2, kill2) <- timedEvent (round (1 * 1000000)) (pure "WHOLE")
            -- Every 2 seconds, fire event 3
            (ev3, kill3) <- timedEvent (round (2 * 1000000)) (pure "DOUBLE")

            -- The String semigroup instance is used in case of simultaneous
            -- events.
            let union1 :: SEvent String
                union1 = ev1 <||> ev2 <||> ev3

            let be1 :: SBehavior String
                be1 = "Initial" |> ev1

            -- If we introduce an SBehavior, then our union becomes an
            -- SBehavior: the initial value is simply the initial value of
            -- the behavior, and it fires whenever any of the events fires.
            let union2 :: SBehavior String
                union2 = be1 <||> ev2 <||> ev3

            sequenceReactimate (const (pure ())) (runIdentity) (putStrLn <$> (((++) "Union 1: ") <$> union1))
            sequenceReactimate (runIdentity) (runIdentity) (putStrLn <$> (((++) "Union 2: ") <$> union2))

            liftIO $ writeIORef killThreads [kill1, kill2, kill3]

    network <- compile networkDescription
    actuate network

    -- Press any key to stop this nonsense.
    getChar
    kills <- readIORef killThreads
    sequenceA kills
    return ()
