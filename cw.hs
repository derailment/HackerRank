import IO.Read
import Data.List as L
import Data.List.Split
import Data.Foldable as F
import Data.Map.Lazy as M

testWords :: [Noun]
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

dic2list :: [Noun] -> [(Size, [(Noun, Slot)])]
dic2list ns = M.toList $ buildDic ns $ fromList []

buildDic :: [Noun] -> M.Map Size [(Noun, Slot)] -> M.Map Size [(Noun, Slot)]
buildDic [] m = m
buildDic (w:ws) m = buildDic ws map'
  where map' = case M.lookup len m
          of Nothing -> M.insert len [(w, EmptySlot)] m
             Just _ -> M.insertWith (++) len [(w, EmptySlot)] m
        len = length w

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
type Noun = String
type Size = Int
type Start = Int
type End = Int
type Id = Int
data Slot = R Id (Start, End) | C Id (Start, End) | EmptySlot deriving (Show)

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
        pair (e:es) = F.foldl (\acc e -> if (last acc) + 1 == e then acc ++ [e] else acc) [e] es
        indices = elemIndices '-' (rs !! i)

row2col :: CrossWords -> CrossWords
row2col rs = [[h !! j| h <- rs] | j <- [0..9]]

















