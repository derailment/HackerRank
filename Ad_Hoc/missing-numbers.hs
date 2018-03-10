import Data.Map.Lazy as M
import Data.List as F
import Data.Foldable

main :: IO ()
main = do
  n' <- getLine
  let n = read n' :: Int
  str <- getLine
  let ns = [ read s :: Int |s <- words str]
  let nsMap = build ns
  m' <- getLine
  let m = read m' :: Int
  str <- getLine
  let ms = [ read s :: Int |s <- words str]
  let msMap = build ms
  forM_ [fst e | e <- M.toList $ M.differenceWith (\v1 v2 -> if v1 == v2 then Nothing else Just (v1 - v2)) msMap nsMap] $ \x ->
    putStr $ (show x) ++ " "

build :: [Int] -> M.Map Int Int
build xs = F.foldl f (M.fromList []) xs
  where f m x = case M.lookup x m of Nothing -> M.insert x 1 m
                                     Just v -> M.insert x (v+1) m



