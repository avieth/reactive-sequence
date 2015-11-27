{-|
Module      : Examples.Bundle
Description : Example of bundling of sequences.
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

            -- Every 1/2 second, fire event 1
            (ev1, kill1) <- timedEvent (round ((1/2) * 1000000)) (pure "HALF")
            -- Every second, fire event 2
            (ev2, kill2) <- timedEvent (round (1 * 1000000)) (pure "WHOLE")
            -- Every 2 seconds, fire event 3
            (ev3, kill3) <- timedEvent (round (2 * 1000000)) (pure "DOUBLE")

            -- We bundle those events, to obtain a new event with the latest
            -- values from all 3 events. It will fire whenever any of them
            -- fires, and all of them have fired at least once.
            let bundle1 :: SEvent (String, String, String)
                bundle1 = (,,) <$> ev1 <%> ev2 <%> ev3

            let be1 :: SBehavior String
                be1 = "initial 1" |> ev1

            let be2 :: SBehavior String
                be2 = "initial 2" |> ev2

            let be3 :: SBehavior String
                be3 = "initial 3" |> ev3

            -- By adding initial values, we can bundle a behavior.
            let bundle2 :: SBehavior (String, String, String)
                bundle2 = (,,) <$> be1 <%> be2 <%> be3

            -- Notice that when we reactimate the SEvent, we can give
            -- const (pure ()) for the first argument, meaning no IO will
            -- happen immediately, as there is no initial value.
            sequenceReactimate (const (pure ())) (runIdentity) (print <$> ((,) "Event" <$> bundle1))
            -- This will fire *many* times before the above reactimate,
            -- for every event of ev1, ev2, or ev3 causes it to run.
            sequenceReactimate (runIdentity) (runIdentity) (print <$> ((,) "Behavior" <$> bundle2))

            liftIO $ writeIORef killThreads [kill1, kill2, kill3]

    network <- compile networkDescription
    actuate network

    -- Press any key to stop this nonsense.
    getChar
    kills <- readIORef killThreads
    sequenceA kills
    return ()
