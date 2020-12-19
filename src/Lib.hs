module Lib
    ( mySum
    ) where

import Mutex

import Control.Concurrent (forkIO, myThreadId)
import Control.Concurrent.Chan (Chan, newChan, readChan, writeChan, getChanContents)
import Control.Monad (forM_)

mySum :: (Show a, Num a) => [[a]] -> IO ()
mySum inputs = do
  let len = length inputs
  mutex <- newMutex
  pSums <- newChan
  -- mapM_ (forkIO . sumThisPart mutex pSums) inputs
  forM_ inputs $ forkIO . sumThisPart mutex pSums
  result <- sum . take len <$> getChanContents pSums
  putStrLn $ "The sum is: " ++ show result

sumThisPart :: (Show a, Num a) => Mutex -> Chan a -> [a] -> IO ()
sumThisPart mutex res nums = do
  lock mutex
  tid <- myThreadId
  putStrLn $ show tid ++ ":\t" ++ show nums
  unLock mutex
  writeChan res $! sum nums

