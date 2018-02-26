import IO.Read
import Data.List
import Data.Function
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    strs <- getLines n'
    let ts = [(read (words str !! 0) :: Int, read (words str !! 1) :: Int) | str <- strs]
    let ((ox, oy) : ps) = sortBy comp' ts
    let ws = sortBy (compare `on` (angle (ox, oy))) ps
    putStrLn $ show ws
    let rs = f [(ox, oy)] ws
    putStrLn $ concave rs ((ox, oy) : ws)

comp' :: (Int, Int) -> (Int, Int) -> Ordering
comp' (ax, ay) (bx, by)
  | ay > by = GT
  | ay < by = LT
  | otherwise = if ax >= bx then GT else LT


concave :: [(Int, Int)] -> [(Int, Int)] -> String
concave rs ps
  | (length ps - length rs) == 0 = "NO"
  | otherwise = "YES"

f :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
f rs [] =  rs
f rs (p : ps)
  | not $ any (outside (last rs) p) ps = f (rs ++ [p]) ps
  | otherwise = f rs ps

outside :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
outside (sx, sy) (ex, ey) (px, py)
  | ((a * px) + (b * py) + c) > 0 = True
  | otherwise = False
  where a = ey - sy
        b = sx - ex
        c = (sy * ex) - (ey * sx)

angle :: (Int, Int) -> (Int, Int) -> Double
angle (ox, oy) (px, py) = acos ((fromIntegral ((ax * bx) + (ay * by))) / (sqrt (fromIntegral ((ax ^ 2) + (ay ^ 2))) * sqrt (fromIntegral ((bx ^ 2) + (by ^ 2)))))
  where ax = 10
        ay = 0
        bx = px - ox
        by = py - oy

