module Lib
    ( someFunc
    ) where

import System.IO
import Control.Concurrent
import Data.Maybe
import Data.List

getChanWhileJust :: Chan (Maybe a) -> IO [a]
-- getChanWhileJust chan = fmap (catMaybes . takeWhile isJust) (getChanContents chan)
getChanWhileJust chan = fmap (catMaybes . take 11) (getChanContents chan)

someFunc :: IO ()
someFunc = do
  io <- newEmptyMVar
  putMVar io ()
  res <- newChan
  calc [1..100] res io
  getChanWhileJust res >>= putStrLn . show . sum

sumUp :: (Show a, Num a) => MVar [a] -> Chan (Maybe a) -> MVar () -> IO ()
sumUp input res io = do
  lst <- takeMVar input
  takeMVar io
  tid <- myThreadId
  putStrLn $ show tid ++ ":\t" ++ show lst
  putMVar io ()
  writeChan res $ Just $ sum lst

calc :: (Show a, Num a) => [a] -> Chan (Maybe a) -> MVar() -> IO ()
calc [] res _ = writeChan res Nothing
calc xs res io = do
  buf <- newEmptyMVar
  putMVar buf . take 10 $ xs
  forkIO $ sumUp buf res io
  calc (drop 10 $ xs) res io
