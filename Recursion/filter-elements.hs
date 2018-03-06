import Data.Char
import Data.Foldable

main :: IO ()
main = do
    n <- getLine
    let n' = read n :: Int
    forM_ [1 .. n'] $ \_ -> do
      line1 <- getLine
      line2 <- getLine
      let numE = read (words line1 !! 0) :: Int
      let numR = read (words line1 !! 1) :: Int
      let xs = map (read :: String -> Int) (words line2)
      let res = map (\x -> show x) (filterE numE numR xs)
      let s = if res == [] then show (- 1) else unwords res
      putStrLn s

filterE :: Int -> Int -> [Int] -> [Int]
filterE ne nr xs =
  let tree = foldl (flip insert) EmptyTree xs in fst $ foldl (searchDrop nr) ([], tree) xs


type Times = Int
type Ele = Int
type Occ = Bool

insert :: Ele -> Tree (Ele, Times, Occ) -> Tree (Ele, Times, Occ)
insert e' EmptyTree = Node (e', 1, True) EmptyTree EmptyTree
insert e' (Node (e, t, p) left right)
  | e' < e = Node (e, t, p) (insert e' left) right
  | e' > e = Node (e, t, p) left (insert e' right)
  | e' == e = Node (e, t + 1, p) left right

searchDrop :: Times -> ([Ele], Tree (Ele, Times, Occ)) -> Ele -> ([Ele], Tree (Ele, Times, Occ))
searchDrop r (xs, EmptyTree) e' = (xs, EmptyTree)
searchDrop r (xs, (Node (e, t, p)  left right)) e'
  | e' == e =
    case p of False -> (xs, Node (e, t, p) left right)
              True -> if t >= r
                then (xs ++ [e], Node (e, t, False) left right)
                else (xs, Node (e, r, False) left right)
  | e' > e = let (xs', right') = searchDrop r (xs, right) e' in (xs', Node (e, t, p) left right')
  | e' < e = let (xs', left') = searchDrop r (xs, left) e' in (xs', Node (e, t, p) left' right)

data Tree a = EmptyTree |  Node a (Tree a) (Tree a) deriving (Show)



