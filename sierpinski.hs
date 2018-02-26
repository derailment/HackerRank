module Sierpinski (
  Position(..),
  Tree(..)
  ) where

import IO.Read
import Data.List
import Data.Function
import Data.Foldable
{-
main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
-}

data Position = Left | Right | Mid
type Direct = (Bool, Bool)
data Tree a b = EmptyTree |  Node a b (Tree a b) (Tree a b) (Tree a b)

build :: Int -> Tree Position Direct

tri :: Int -> [Int]
tri i = [if 31 - j <= i && 31 + j >= i then 1 else 0 | j <- [0..31], i <- [0..62]]

printTri :: [Int] -> Int -> Int -> IO()
printTri ls r c  =
  forM_ [0..(r-1)] $ \r' -> do
    forM_ [0..(c-1)] $ \c' -> do
      putStr $ show (ls !! ((r' * c) + c'))
    putStrLn ""







