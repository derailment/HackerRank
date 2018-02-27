import IO.Read
import Data.List
import Data.Function
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    let res = sortBy comp' (f n' (31, 0) (0, 31) (62, 31))
    forM_ [0..31] $ \j -> do
      forM_ [0..62] $ \i -> do
        putStr $ if (i, j) `elem` res then "1" else "_"
      putStrLn ""

comp' :: (Int, Int) -> (Int, Int) -> Ordering
comp' (ax, ay) (bx, by)
  | ax > bx = GT
  | ax < bx = LT
  | otherwise = if ay >= by then GT else LT

f :: Int -> (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
f 0 (ax, ay) (bx, by) (cx, cy) = unit (ax, ay) (bx, by) (cx, cy)
f n (ax, ay) (bx, by) (cx, cy) = top ++ left ++ right
  where top = f (n-1) (ax, ay) ((ax + bx + 1) `div` 2, (ay + cy - 1) `div` 2) ((ax + cx - 1) `div` 2, (ay + cy - 1) `div` 2)
        left = f (n-1) ((ax + bx - 1) `div` 2, (ay + cy + 1) `div` 2) (bx, by) ((bx + cx) `div` 2 - 1, by)
        right = f (n-1) ((ax + cx + 1) `div` 2, (ay + cy + 1) `div` 2) (((bx + cx) `div` 2) + 1, cy) (cx, cy)


unit :: (Int, Int) -> (Int, Int) -> (Int, Int) -> [(Int, Int)]
unit (ax, ay) (bx, by) (cx, cy) = filter (inside (ax, ay) (bx, by) (cx, cy)) [(x,y) | x <- [(min . min ax) bx cx..(max . max ax) bx cx], y <- [(min . min ay) by cy..(max . max ay) by cy]]

inside :: (Int, Int) -> (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool
inside (ax, ay) (bx, by) (cx, cy) (px, py) =
  same (outside (ax, ay) (bx, by) (cx, cy)) (outside (ax, ay) (bx, by) (px, py))
  && same (outside (ax, ay) (cx, cy) (bx, by)) (outside (ax, ay) (cx, cy) (px,py))
  && same (outside (bx, by) (cx, cy) (ax, ay)) (outside (bx, by) (cx, cy) (px, py))

-- check if two points are on the same side of triangle
same :: Pos -> Pos -> Bool
same _ Onside = True
same Onside _ = True
same a b
  | a == b = True
  | otherwise = False

data Pos = Inside | Onside | Outside deriving (Eq)

outside :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Pos
outside (sx, sy) (ex, ey) (px, py)
  | ((a * px) + (b * py) + c) == 0 = Onside
  | ((a * px) + (b * py) + c) > 0 = Outside
  | otherwise = Inside
  where a = ey - sy
        b = sx - ex
        c = (sy * ex) - (ey * sx)








