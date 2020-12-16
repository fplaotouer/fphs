module Lib
    ( runDemo
    ) where

import Mutex

import System.IO
import Control.Concurrent
import Data.Maybe
import Data.List

getChanWhileJust :: Chan (Maybe a) -> IO [a]
-- getChanWhileJust chan = fmap (catMaybes . takeWhile isJust) (getChanContents chan)
getChanWhileJust chan = fmap (catMaybes . take 11) (getChanContents chan)

runDemo :: IO ()
runDemo = do
  mutex <- newMutex
  res <- newChan
  calc [1..100] res mutex
  getChanWhileJust res >>= putStrLn . show . sum

sumUp :: (Show a, Num a) => MVar [a] -> Chan (Maybe a) -> Mutex -> IO ()
sumUp input res mutex = do
  lst <- takeMVar input
  lock mutex
  tid <- myThreadId
  putStrLn $ show tid ++ ":\t" ++ show lst
  unLock mutex
  writeChan res $ Just $ sum lst

calc :: (Show a, Num a) => [a] -> Chan (Maybe a) -> Mutex -> IO ()
calc [] res _ = writeChan res Nothing
calc xs res mutex = do
  buf <- newEmptyMVar
  putMVar buf . take 10 $ xs
  forkIO $ sumUp buf res mutex
  calc (drop 10 $ xs) res mutex
