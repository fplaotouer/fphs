module Lib
    ( mySum
    ) where

import Control.Concurrent (forkIO)
import Data.List.Split (chunksOf)

import qualified Control.Concurrent.STM as S


type Result = S.TVar (Int, Int)

mySum :: Int -> Int -> Int -> IO ()
mySum lower upper chunkSize = do
    let chunks = chunksOf chunkSize [lower..upper]
    let total = length chunks
    result <- S.atomically $ S.newTVar (0, 0)
    mapM_ (\chunk -> forkIO $ S.atomically $ addToResult result chunk) chunks
    value <- S.atomically $ getResult result total
    putStrLn $ "The result is " ++ show value

addToResult :: Result -> [Int] -> S.STM ()
addToResult result chunk = do
    (value, finished) <- S.readTVar result
    S.writeTVar result (value + sum chunk, finished + 1)

getResult :: Result -> Int -> S.STM Int
getResult result total = do
    (value, finished) <- S.readTVar result
    if finished < total then S.retry else return value
