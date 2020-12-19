module Mutex
    ( Mutex
    , newMutex
    , lock
    , unLock
    ) where

import Control.Concurrent.MVar (MVar, newMVar, takeMVar, putMVar)

type Mutex = MVar ()

newMutex :: IO Mutex
newMutex = newMVar ()

lock :: Mutex -> IO ()
lock = takeMVar

unLock :: Mutex -> IO ()
unLock = flip putMVar ()
