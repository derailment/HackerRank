import Data.List
import Data.Foldable

getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)

main :: IO ()
main = do
  n <- getLine
  let n' = read n :: Int
  xs <- getLines n'
  forM_ [0 .. (n' - 1)] $ \i -> do
    let str = xs !! i
    putStrLn $ show (foc str)

foc :: [Char] -> Bool
foc str = count 0 0 0 0 str where
  count r g y b [] = r == g && y == b
  count r g y b (x : xs)
    | x == 'R' = abs (r + 1 - g) <= 1 && count (r + 1) g y b xs
    | x == 'G' = abs (g + 1 - r) <= 1 && count r (g + 1) y b xs
    | x == 'Y' = abs (y + 1 - b) <= 1 && count r g (y + 1) b xs
    | x == 'B' = abs (b + 1 - y) <= 1 && count r g y (b + 1) xs

