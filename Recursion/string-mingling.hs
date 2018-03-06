import Data.Foldable

main :: IO ()
main = do
    n1 <- getLine
    n2 <- getLine
    let xs = concat [[f, b] | (f, b) <- (zip n1 n2)]
    putStrLn xs

