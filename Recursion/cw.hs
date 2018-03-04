import IO.Read
import Data.List as L
import Data.List.Split
import Data.Foldable as F
import Data.Map.Lazy as M

testWords :: [Noun]
testWords = splitOn ";" "ANKARA;LONDON;DELHI;ICELAND"

testCrossWords :: CrossWords
testCrossWords =
  ["+-++++++++",
   "+-++++++++",
   "+-++++++++",
   "+-----++++",
   "+-+++-++++",
   "+-+++-++++",
   "+++++-++++",
   "++------++",
   "+++++-++++",
   "+++++-++++"]

testDic = pasteSlots (findSlots testCrossWords) (buildDic testWords (fromList []))

testValid = validate testCrossWords testDic

testSolve = solve testCrossWords testWords

type CrossWords = [String]
type Noun = String
type Size = Int
type Start = Int
type End = Int
type Id = Int
data Slot = R Id (Start, End) | C Id (Start, End) | EmptySlot deriving (Show, Eq)

solve :: CrossWords -> [Noun] -> CrossWords
solve cw ws = case head $ L.filter (/= Nothing) [validate cw (pasteSlots ss (buildDic ws (fromList []))) | ss <- permu]
  of Just cw' -> cw'
  where slots = findSlots cw
        permu = permutations slots

validate :: CrossWords -> M.Map Size [(Noun, Slot)] -> Maybe CrossWords
validate cw m = f (Just cw) (M.toList m)
  where f Nothing _ = Nothing
        f (Just cw) [] = Just cw
        f (Just cw) ((size, hs) : ms) = f (f' (Just cw) hs) ms
          where f' Nothing _ = Nothing
                f' (Just cw) [] = Just cw
                f' (Just cw) ((n, R i (s, e)) : ns) = f' (fill cw i (s, e) n) ns
                f' (Just cw) ((n, C i (s, e)) : ns) = case tcw
                  of Nothing -> Nothing
                     Just tcw' -> f' (Just (trans tcw')) ns
                  where tcw = (fill (trans cw) i (s, e) n)


fill cw i (s, e) n = case any (\(a, b) -> a /= '-' && a /= b) [(a, b) | (a, b) <- zip l n]
                       of True -> Nothing
                          False -> Just (paste cw i (s, e) n)
  where l = take (e - s + 1) $ drop s (cw !! i)
        paste cw i (s, e) n = let (fs, (b:bs)) = splitAt i cw;
                                       l' = b;
                                       fs' = take s l';
                                       bs' = drop (e + 1) l';
                                       n' = fs' ++ n ++ bs';
                                    in fs ++ [n'] ++ bs

pasteSlots :: [Slot] -> M.Map Size [(Noun, Slot)] -> M.Map Size [(Noun, Slot)]
pasteSlots [] m = m
pasteSlots (l : ls) m = pasteSlots ls (M.insert size (p (getPairs size m) l) m)
  where getPairs size m = case M.lookup size m
                            of Nothing -> []
                               Just ns -> ns
        getSE (R i (s, e)) = (s, e)
        getSE (C i (s, e)) = (s, e)
        p ((n, s) : ns) slot = if s == EmptySlot then (n, slot) : ns else (n, s) : p ns slot
        size = let (s, e) = getSE l in e - s + 1

findSlots :: CrossWords -> [Slot]
findSlots cw = concat [findSlot R i cw ++ findSlot C i (trans cw) | i <- [0..9]]

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

trans :: CrossWords -> CrossWords
trans rs = [[h !! j| h <- rs] | j <- [0..9]]

buildDic :: [Noun] -> M.Map Size [(Noun, Slot)] -> M.Map Size [(Noun, Slot)]
buildDic [] m = m
buildDic (w : ws) m = buildDic ws m'
  where m' = case M.lookup len m
                 of Nothing -> M.insert len [(w, EmptySlot)] m
                    Just _ -> M.insertWith (++) len [(w, EmptySlot)] m
        len = length w


main :: IO ()
main = do
  cw <- getLines 10
  lastline <- getLine
  let ws = splitOn ";" lastline
  forM_ (solve cw ws) $ \s -> do
    putStrLn s




















