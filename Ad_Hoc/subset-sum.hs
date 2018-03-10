import Data.Foldable
import Data.Set as S
import Data.List

getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)

main = do
  n' <- getLine
  str <- getLine
  let as = [read x :: Int | x <- words str]
  let accs = scanl1 (+) $ sortBy (flip compare) as
  let ass = S.fromList accs
  m' <- getLine
  let m = read m' :: Int
  forM_ [1..m] $ \_ -> do
    s <- readLn :: IO Int
    if s > last accs
       then print (-1)
       else print $ f s ass

f :: Int -> Set Int -> Int
f s ass = let (lessS, greaterS) = split s ass in (size lessS) + 1




