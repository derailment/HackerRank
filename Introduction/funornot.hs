import Data.Foldable
import IO.Read

main :: IO ()
main = do 
    ctemp <- getLine
    let caseN = read ctemp :: Int
    forM_ [1 .. caseN] $ \_ -> do
        ptemp <- getLine
        let pointN = read ptemp :: Int
        strs <- getLines pointN
        let ts = [(read (words str !! 0) :: Int, read (words str !! 1) :: Int) | str <- strs]
        putStrLn $ valFun ts

valFun :: [(Int, Int)] -> String
valFun [] = "YES"
valFun ((hx, hy) : ts) = 
    if valFun ts == "NO" 
        then  "NO"
        else if any (== "NO") [ "NO" |(x, y) <- ts, (hx == x) && (hy /= y)]
            then "NO"
            else "YES"

