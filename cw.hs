import IO.Read
import Data.List
import Data.List.Split
import Data.Function
import Data.Foldable
import Control.Applicative
import Control.Monad

testWords :: [String]
testWords = splitOn ";" "LONDON;DELHI;ICELAND;ANKARA"

testCrossWords :: CrossWords
testCrossWords =
  ["+-++++++++",
   "+-++++++++",
   "+-------++",
   "+-++++++++",
   "+-++++++++",
   "+------+++",
   "+-+++-++++",
   "+++++-++++",
   "+++++-++++",
   "++++++++++"]

{-
main :: IO ()
main = do
  hs <- getLines 10
  vs <- zipHor hs
  endline <- getLine
  let ws = splitOn ";" endline
  putStrLn $ show vs
-}

type CrossWords = [String]
type Start = Int
type End = Int
type Id = Int
data Slot = R Id (Start, End) | C Id (Start, End) deriving (Show)

findSlots :: CrossWords -> [Slot]
findSlots cw = concat [findSlot R i cw ++ findSlot C i (row2col cw) | i <- [0..9]]

findSlot vh i rs = seg indices
  where seg [] = []
        seg is
          | length rest /= 0 = if len > 1 then [vh i (head succ, last succ)] ++ seg rest else seg rest
          | otherwise = if len > 1 then [vh i (head succ, last succ)] else []
          where succ = pair is
                rest = drop len is
                len = length succ
        pair (e:es) = foldl (\acc e -> if (last acc) + 1 == e then acc ++ [e] else acc) [e] es
        indices = elemIndices '-' (rs !! i)

row2col :: CrossWords -> CrossWords
row2col rs = [[h !! j| h <- rs] | j <- [0..9]]
















