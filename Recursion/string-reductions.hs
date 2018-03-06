import Data.List
import Data.Foldable

main :: IO ()
main = do
    str <- getLine
    putStrLn $ red [] str

red :: [Char] -> [Char] -> [Char]

red res (x : xs)
  | x `elem` res = red res xs
  | otherwise = red (res ++ [x]) xs

red res [] = res

