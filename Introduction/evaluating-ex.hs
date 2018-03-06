import Control.Applicative
import Control.Monad
import System.IO


main :: IO ()
main = do
    n_temp <- getLine
    let n = read n_temp :: Int
    forM_ [1..n] $ \a0  -> do
        x_temp <- getLine
        let x = read x_temp :: Double
        putStrLn $ show (f x)

f :: Double -> Double
f x = foldr (\y acc -> acc + ((x ^ y) / fromInteger (fac y))) 0 [0..9] 

fac :: Integer -> Integer
fac 0 = 1
fac y = y * fac (y - 1)
