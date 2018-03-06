import Data.List
import Data.Function
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
    strs <- getLines n'
    forM_ strs $ \s -> do
      putStrLn $ unwords $ f s (length s)

f :: [Char] -> Int -> [String]
f rs 0 = []
f (s:ss) n = let next = ss ++ [s] in [next] ++ f next (n - 1)
