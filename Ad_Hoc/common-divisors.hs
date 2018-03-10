import Data.Foldable

getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)


main = do
  n' <- getLine
  let n = read n' :: Int
  forM_ [1..n] $ \i -> do
    str <- getLine
    let (x, y) = (read (words str !! 0) :: Int, read (words str !! 1) :: Int)
    let greatest = gcd x y
    print $ length $ filter (== True)[x `mod` d == 0 && y `mod` d == 0 | d <- [1..greatest]]

