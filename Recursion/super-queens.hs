import Data.List
import Data.Foldable
import Control.Monad

queens :: Int -> [[Int]]
queens n = foldM f [] [1..n]
  where f qsc _ = [qc:qsc | qc <- ([1..n] \\ qsc), isSafe n qc qsc ]

isSafe :: Int -> Int -> [Int] -> Bool
isSafe n c qsc = go c r qsc qsr
  where qsr = [lenq, lenq - 1..1]
        r = lenq + 1
        lenq = length qsc
        go _ _ [] [] = True
        go c' r' (qc':qcs') (qr':qrs')
          | c' == qc' = False
          | otherwise = notdiag (c', r') (qc', qr') && notknight (c', r') (qc', qr') && go c' r' qcs' qrs'

notdiag :: (Int, Int) -> (Int, Int) -> Bool
notdiag (c, r) (qc, qr)
  | abs (fromIntegral (r - qr) / fromIntegral (c - qc)) == 1 = False
  | otherwise = True

notknight :: (Int, Int) -> (Int, Int) -> Bool
notknight (c, r) (qc, qr) = not ((c, r) `elem` [(qc + 2, qr + 1), (qc + 2, qr - 1), (qc + 1, qr + 2), (qc + 1, qr - 2), (qc - 2, qr + 1), (qc - 2, qr - 1), (qc - 1, qr + 2), (qc - 1, qr - 2)])

main :: IO ()
main = do
  n' <- getLine
  let n = read n' :: Int
  putStrLn $ show $ length $ queens n




















