import Control.Monad
import Data.Foldable

main :: IO ()
main = do
    x' <- getLine
    let x = read x' :: Float
    n' <- getLine
    let n = read n' :: Float
    let y = floor $ x ** ((1 :: Float) / n)
    let ys = map (^ (round n)) [1 .. y]
    let res = f (round x) ys
    putStrLn $ show res


f :: Int ->  [Int] -> Int
f _ [] = 0
f t (x:xs)
  | x > t = f t xs
  | x == t = 1
  | x < t = f (t - x) xs + f t xs




