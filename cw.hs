import IO.Read
import Data.List
import Data.List.Split
import Data.Function
import Data.Foldable
import Control.Applicative
import Control.Monad
main :: IO ()
main = do
  hs <- getLines 10
  vs <- zipHor hs
  endline <- getLine
  let ws = splitOn ";" endline
  putStrLn $ show vs


zipHor :: [String] -> IO [String]
zipHor hs = forM [0..length hs - 1] $ \i -> do
  let v = [h !! i | h <- hs]
  return v














