import Data.List
import Data.Function
import Data.Foldable

getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    strs <- getLines n'
    let ts = [(read (words str !! 0) :: Int, read (words str !! 1) :: Int) | str <- strs]
    let ((ox, oy) : ps) = sortBy comp' ts
    let ws = sortBy (compare `on` (angle (ox, oy))) ps
    let rs = f [(ox, oy)] ws
    let p = peri rs (ox, oy)
    putStrLn $ show p

comp' :: (Int, Int) -> (Int, Int) -> Ordering
comp' (ax, ay) (bx, by)
  | ay > by = GT
  | ay < by = LT
  | otherwise = if ax >= bx then GT else LT

peri :: [(Int, Int)] -> (Int, Int) -> Double
peri ((ax, ay) : []) (bx, by) = sqrt (fromIntegral ((ax - bx) ^ 2) + fromIntegral ((ay - by) ^ 2))
peri ((ax, ay) : (bx, by) : rs) (ox, oy) = sqrt (fromIntegral ((ax - bx) ^ 2) + fromIntegral ((ay - by) ^ 2)) + peri ((bx, by) : rs) (ox, oy)

f :: [(Int, Int)] -> [(Int, Int)] -> [(Int, Int)]
f rs [] =  rs
f rs (p : ps)
  | not $ any (outside (last rs) p) ps = f (rs ++ [p]) ps
  | otherwise = f rs ps

outside :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
outside (sx, sy) (ex, ey) (px, py)
  | ((a * px) + (b * py) + c) >= 0 = True
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

