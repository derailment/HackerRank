import Data.Foldable as F

getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)

main = do
  n <- readLn :: IO Int
  strs <- getLines n
  F.forM_ strs $ \str -> do
    let nk = words str
    let (n, k) = (read (nk !! 0) :: Int, read (nk !! 1) :: Int)
    print $ (allways !! n !! k) `mod` 100000007


allways :: [[Integer]]
allways = [[ count n k | k <- [0..1000]] | n <- [0..]]
    where count _ 0 = 1
          count n k
            | k == n = 1
            | otherwise = allways !! (n-1) !! (k-1) + allways !! (n-1) !! k