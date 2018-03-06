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
    let ts' = (hx, hy) : ts
    putStrLn $ show $ perPoly (hx, hy) ts'

perPoly :: (Int, Int) -> [(Int, Int)] -> Double
perPoly (hx, hy) [(x0, y0)] = dis (hx, hy) (x0, y0)
perPoly (hx, hy) ((x0, y0) : (x1, y1) : xs) = dis (x0, y0) (x1, y1) + perPoly (hx, hy) ((x1, y1) : xs)

dis :: (Int, Int) -> (Int, Int) -> Double
dis (x0, y0) (x1, y1) = sqrt $ fromIntegral $ ( (-) x0 x1) ^ 2 + ( (-) y0 y1) ^ 2




