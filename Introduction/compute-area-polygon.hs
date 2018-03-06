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
    let ((hx, hy) : ts) = [(read (words str !! 0) :: Int, read (words str !! 1) :: Int) | str <- strs]
    putStrLn $ show $ areaPoly (hx, hy) ts

areaPoly :: (Int, Int) -> [(Int, Int)] -> Double
areaPoly (hx, hy) [(x0, y0)] = 0
areaPoly (hx, hy) ((x0, y0) : (x1, y1) : xs)
    | orient (hx, hy) (x0, y0) (x1, y1) = subtract (triA (hx, hy) (x0, y0) (x1, y1)) (areaPoly (hx, hy) ((x1, y1) : xs))
    | otherwise = triA (hx, hy) (x0, y0) (x1, y1) + areaPoly (hx, hy) ((x1, y1) : xs)

orient :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Bool -- True for counterclockwise
orient (x0, y0) (x1, y1) (x2, y2) =
    if (y2 - y1) * (x1 - x0) - (y1 - y0) * (x2 - x1) < 0
        then True
        else False

triA :: (Int, Int) -> (Int, Int) -> (Int, Int) -> Double
triA (x0, y0) (x1, y1) (x2, y2)= abs (fromIntegral $ (x0 * y1) + (x1 * y2) + (x2 * y0) - (x1 * y0) - (x2 * y1) - (x0 * y2)) / 2




