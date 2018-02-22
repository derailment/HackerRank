import Data.Char
import Data.Foldable

main :: IO ()
main = do
    line <- getLine
    let n' = words line !! 0
    let k = read (words line !! 1) :: Int
    let res = foldl (f k) 0 n'
    putStrLn $ show res

f :: Int -> Int -> Char -> Int
f k acc x' = let x = digitToInt x' in sd $ x * k + acc

sd x
  | x < 10 = x
  | otherwise = sd $ x `mod` 10 + (sd (x `div` 10))


