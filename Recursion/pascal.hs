import IO.Read
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    forM_ [0 .. (n' - 1)] $ \row -> do
      forM_ [0 .. row] $ \col -> do
        putStr $ show (ele row col)  ++ " "
      putStrLn ""

ele :: Int -> Int -> Int
ele n r = fac n `div` ((fac r) * (fac (n - r)))

fac 0 = 1
fac x = x * (fac (x - 1))
