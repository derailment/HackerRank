import Data.Foldable as F
import Data.Vector.Generic.Mutable as VM


getLines :: Int -> IO [Int]
getLines n
    | n <= 0 = return []
    | otherwise = do
      str <- getLine
      let thisLn = Prelude.read str :: Int
      others <- getLines (n - 1)
      return (thisLn : others)
{-
main = do
  n <- readLn :: IO Int
  xs <- getLines n
  F.forM_ xs $ \x -> do
    print $ vp ! (x - 1)
-}

--vp :: IO ()
vp = do
  v <- VM.new 100000
  VM.unsafeWrite v 0 1
{-
  where fill v = map pen [1..99999]
        pen x = do
          GM.unsafeWrite v px
          return px
            where px = GM.unsafeRead v (x-1) + (3*(x)+1)
-}

