{-|
Module      : Examples.Switch
Description : Example of sequence switching.
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
import Data.Semigroup
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
            -- Every 7 seconds, produce ev1.
            (ev3, kill3) <- timedEvent (round (7 * 1000000)) (pure ev1)
            -- Every 3 seconds, produce ev2.
            (ev4, kill4) <- timedEvent (round (3 * 1000000)) (pure ev2)

            -- The First semigroup instance is used in case of simultaneous
            -- events.
            let ev :: SEvent (SEvent String)
                ev = getFirst <$> ((First <$> ev3) <||> (First <$> ev4))

            ev' :: SEvent String
                <- runSequenceM $ switch const ev

            runSequenceM $
                sequenceReactimate (const (pure ()))
                                   (runIdentity)
                                   (putStrLn <$> (((++) "Event: ") <$> ev'))

            liftIO $ writeIORef killThreads [kill1, kill2, kill3, kill4]

    network <- compile networkDescription
    actuate network

    -- Press any key to stop this nonsense.
    getChar
    kills <- readIORef killThreads
    sequenceA kills
    return ()
