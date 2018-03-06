import Data.List
import Data.Function
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    let res = f n' 16 (49, 62)
    forM_ [0..62] $ \j -> do
      forM_ [0..99] $ \i -> do
        putStr $ if (i, j) `elem` res then "1" else "_"
      putStrLn ""

comp' :: (Int, Int) -> (Int, Int) -> Ordering
comp' (ax, ay) (bx, by)
  | ax > bx = GT
  | ax < bx = LT
  | otherwise = if ay >= by then GT else LT

f :: Int -> Int -> (Int, Int) -> [(Int, Int)]
f 0 _ _ = []
f _ 1 (ox, oy) = unit (ox, oy) 1
f iter len (ox, oy) = f (iter - 1) (len `div` 2) (lx, ly - 1) ++ f (iter - 1) (len `div` 2) (rx, ry - 1) ++ base
  where base = unit (ox, oy) len
        ((lx, ly), (rx, ry)) = cor base

cor :: [(Int, Int)] -> ((Int, Int), (Int, Int))
cor tree = let tree' = sortBy comp' tree in (head tree', last tree')


unit :: (Int, Int) -> Int -> [(Int, Int)]
unit (ox, oy) len = let trunk = [(ox, y) | y <- [(oy - len + 1)..oy]];
                        (nx , ny) = head trunk;
                        rightBran = foldl fr [(nx+1, ny-1)] (take (len-1) (repeat 1));
                        leftBran = foldl fl [(nx-1, ny-1)] (take (len-1) (repeat 1));
                    in trunk ++ rightBran ++ leftBran
                    where fr = \acc delta -> acc ++ [((fst . last) acc + delta, (snd . last) acc - delta)]
                          fl = \acc delta -> acc ++ [((fst . last) acc - delta, (snd . last) acc - delta)]









