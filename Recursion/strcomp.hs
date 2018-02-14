import IO.Read
import Data.List
import Data.Foldable

main :: IO ()
main = do
    str <- getLine
    putStrLn $ compress str

compress :: [Char] -> [Char]
compress xs = let ys = group xs in concat [[c] ++ (if n /= 1 then show n else "") | (c, n) <- zip (map head ys) (map length ys)]
