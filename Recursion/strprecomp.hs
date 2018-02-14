import IO.Read
import Data.List
import Data.Foldable

main :: IO ()
main = do
    str1 <- getLine
    str2 <- getLine
    let (len, pre) = getPreLen str1 str2
    putStrLn $ (show len) ++ " " ++ pre
    putStrLn $ unwords $ precomp len str1
    putStrLn $ unwords $ precomp len str2

getPreLen :: [Char] -> [Char] -> (Int, [Char])
getPreLen str1 str2 =
  let len = length $ takeWhile (== True) [if x == y then True else False | (x, y) <- zip str1 str2] in (len, take len str1)

precomp :: Int -> [Char] -> [[Char]] -- return [['3', 'k', 'i', 't']]
precomp n str = let rest = drop n str in [(show (length rest)), rest]



