import Data.List
main :: IO ()
main = do
  n' <- getLine
  let n = read n' :: Int
  str <- getLine
  let ns = [ read s :: Integer |s <- words str]
  let a = foldl (\acc x -> acc * x) 1 ns
  m' <- getLine
  let m = read m' :: Int
  str <- getLine
  let ms = [ read s :: Integer |s <- words str]
  let b = foldl (\acc x -> acc * x) 1 ms
  print $ (gcd a b) `mod` 1000000007



