
getLines :: Int -> IO [String]
getLines n
    | n <= 0 = return []
    | otherwise = do
        thisLn <- getLine
        others <- getLines (n - 1)
        return (thisLn : others)

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show)

build :: Tree a -> a -> a -> a-> Tree a
build (Node x left right) i l r
  | x == i = Node x (Node l Empty Empty) (Node r Empty Empty)
  | find left i l r == Nothing = find right i l r
  | find right i l r
    where find Empty _ _ _ = Nothing
          find tree i l r =  build tree i l r



