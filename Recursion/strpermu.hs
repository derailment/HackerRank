import IO.Read
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    xs <- getLines n'
    forM_ [0 .. (n' - 1)] $ \p -> do
    let str = xs !! p
    putStrLn $ permute str

permute :: [Char] -> [Char]
permute [] = []
permute (f : s : xs) = (s : f : permute xs)
