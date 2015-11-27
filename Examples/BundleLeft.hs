{-|
Module      : Examples.BundleLeft
Description : Example of left-bundling of sequences.
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

            ref <- liftIO $ newIORef (0 :: Int)

            -- Every 1/2 second, fire event 1
            (ev1 :: SEvent (), kill1) <- timedEvent (round ((1/2) * 1000000))
                                                    (modifyIORef ref ((+) 1))
            -- Every second, fire event 2
            (ev2 :: SEvent Int, kill2) <- timedEvent (round (1 * 1000000))
                                                     (readIORef ref)
            -- Every 2 seconds, fire event 3
            (ev3 :: SEvent String, kill3) <- timedEvent (round (2 * 1000000))
                                                        (pure "DOUBLE")

            -- This should fire only when the 2-second event fires, with
            -- the latest values from the other events.
            -- Since the ioref is incremented twice per second, and is read
            -- every second, we ought to see 4*n printed for every n. But this
            -- is concurrent computing: we'll actually see 4*n + 1 since the
            -- third event will not fire exactly at the same time as the second
            -- event.
            let bundle1 :: SEvent ((), Int, String)
                bundle1 = (,,) <$> ev1 <% ev2 <% ev3

            sequenceReactimate (const (pure ()))
                               (runIdentity)
                               (print <$> ((,) "Event 1" <$> bundle1))

            -- This one will fire every half second once both events have fired
            -- at least once, with the latest from both events.
            -- Since @ev2@ reads the value, and fires slightly after ev1, we'll
            -- miss all even numbers.
            let bundle2 :: SEvent ((), Int)
                bundle2 = (,) <$> ev1 %> ev2

            sequenceReactimate (const (pure ()))
                               (runIdentity)
                               (print <$> ((,) "Event 2" <$> bundle2))

            liftIO $ writeIORef killThreads [kill1, kill2, kill3]

            return ()

    network <- compile networkDescription
    actuate network

    -- Press any key to stop this nonsense.
    getChar
    kills <- readIORef killThreads
    sequenceA kills
    return ()
