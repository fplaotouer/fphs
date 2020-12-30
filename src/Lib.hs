module Lib
    ( mySum
    ) where

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, getChanContents)
import Control.Monad (forM_)

import qualified Mutex as M


mySum :: (Show a, Num a) => [[a]] -> IO ()
mySum inputs = do
    let len = length inputs
    mutex <- M.newMutex
    pSums <- newChan
    -- mapM_ (forkIO . sumThisPart mutex pSums) inputs
    forM_ inputs $ forkIO . sumThisPart mutex pSums
    result <- sum . take len <$> getChanContents pSums
    putStrLn $ "The sum is: " ++ show result

sumThisPart :: (Show a, Num a) => M.Mutex -> Chan a -> [a] -> IO ()
sumThisPart mutex res nums = do
    M.lock mutex
    tid <- myThreadId
    putStrLn $ show tid ++ ":\t" ++ show nums
    M.unLock mutex
    writeChan res $! sum nums

