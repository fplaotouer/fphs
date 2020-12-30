module Main where

import Data.List.Split (chunksOf)

import Lib


main :: IO ()
main = mySum . chunksOf 10 $ [1..100]
