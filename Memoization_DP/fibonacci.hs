import Data.Foldable


getLines :: Int -> IO [Int]
getLines n
    | n <= 0 = return []
    | otherwise = do
      thisLn <- readLn :: IO Int
      others <- getLines (n - 1)
      return (thisLn : others)

main = do
  n <- readLn :: IO Int
  xs <- getLines n
  forM_ xs $ \x -> do
    print $ m_fib x `mod` 100000007

m_fib :: Int -> Integer
m_fib = (map fib [0..] !!)
  where fib 0 = 0
        fib 1 = 1
        fib n = m_fib (n-2) + m_fib (n-1)
