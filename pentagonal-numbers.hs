import Data.Foldable as F
import Data.Vector as V


getLines :: Int -> IO [Int]
getLines n
    | n <= 0 = return []
    | otherwise = do
      str <- getLine
      let thisLn = read str :: Int
      others <- getLines (n - 1)
      return (thisLn : others)

main = do
  n <- readLn :: IO Int
  xs <- getLines n
  F.forM_ xs $ \x -> do
    print $ m_pen x

m_pen :: Int -> Int
m_pen = (\x -> Prelude.map pen [1..] !! (x - 1))
  where pen 1 = 1
        pen n = (3 * (n - 1) + 1) + m_pen (n-1)
