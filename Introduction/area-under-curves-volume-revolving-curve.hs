import Text.Printf (printf)

-- This function should return a list [area, volume].
solve :: Int -> Int -> [Int] -> [Int] -> [Double]
solve l r as bs = 
    let area = sum [s x as bs | x <- [((fromIntegral l) + 0.0005), ((fromIntegral l) + 0.0015) .. (fromIntegral r)]]
        volumn = sum [0.001 * pi * ((f x as bs) ^ 2) | x <- [((fromIntegral l) + 0.0005), ((fromIntegral l) + 0.0015) .. (fromIntegral r)]]
        in [area, volumn]
    where s :: Double -> [Int] -> [Int] -> Double
          s x as bs = (f x as bs) * 0.001
          f :: Double -> [Int] -> [Int] -> Double
          f x as bs = sum [((fromIntegral a) * (x ** fromIntegral(b))) | (a, b) <- zip as bs]
    

-- Input/Output
main :: IO ()
main = getContents >>= mapM_ (printf "%.1f\n"). (\[a, b, [l, r]] -> solve l r a b). map (map read. words). lines

