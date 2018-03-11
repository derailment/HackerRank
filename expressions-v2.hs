{-
  Expression :: = Term [+-] Expression
                | Term
  Term       :: = Factor [*/] Term
                | Factor
  Factor     :: = Number
                | [+-] Factor
                | '(' Expression ')'
-}

import Data.Char
{-
newtype Parser a = Parser { parse :: String -> (a, String) }

instance MonadS Parser where
  return' a = Parser (\s -> (a, s))
  (>>=*) p f = Parser (\s' -> parse (f (fst $ parse p s')) s')
  fail' _ = Parser (\s -> ('#', []))

class MonadS m where
  return' :: a -> m a
  (>>=*) :: m a -> (a -> m b) -> m b
  fail' :: String -> m Char

takeOne :: Parser Char
takeOne = Parser $ \s ->
  case s
    of [] -> ('#', [])
       (c : cs) -> (c, cs)

satisfy :: (Char -> Bool) -> Parser Char
satisfy p = takeOne >>=* \c ->
  if p c
     then return' c
     else Parser (\s -> ('#', []))

-- parse (satisfy isDigit) "1234" = ('1', "1234")

option :: Parser Char -> Parser Char -> Parser Char
option p q = Parser $ \s ->
               case parse p s
                 of ('#', []) -> parse q s
                    res -> res

number :: Parser Int
-}

{- Reverse Polish Notation

testRPN = rpn ["4", "-2", "/", "2", "/", "8", "+"]

-}

rpn :: [String] -> Double
rpn ss = read (head $ foldl f [] ss) :: Double
  where f (x:y:zs) "+" = show ((read y :: Double) + (read x :: Double)):zs
        f (x:y:zs) "-" = show ((read y :: Double) - (read x :: Double)):zs
        f (x:y:zs) "*" = show ((read y :: Double) * (read x :: Double)):zs
        f (x:y:zs) "/" = show ((read y :: Double) / (read x :: Double)):zs
        f zs number = number:zs

{- infix to postfix

testToPostfix = toPostfix ["(", "1", "+", "2", ")", "*", "(", "1", "/", "2", ")"]
testToPostfix2 = toPostfix ["1", "/", "2", "/", "3", "/", "4"]
testToPostfix3 = toPostfix ["4", "/", "2", "/", "-2", "+", "8"]

-}

toPostfix :: [String] -> [String]
toPostfix ss = let (op, stack) = foldl f ([], []) ss
                 in if not $ null op then stack ++ op else stack
  where f (op, stack) "(" = ("(" : op, stack)
        f (op, stack) "+" = comp (op, stack) "+"
        f (op, stack) "-" = comp (op, stack) "-"
        f (op, stack) "*" = comp (op, stack) "*"
        f (op, stack) "/" = comp (op, stack) "/"
        f (op, stack) ")" = (drop 1 restOp, stack ++ reverse inparenOp)
          where (inparenOp, restOp) = break (== "(") op
        f (op, stack) number = (op, stack ++ [number])

comp :: ([String], [String]) -> String -> ([String], [String])
comp ([], stack) incomeOp = ([incomeOp], stack)
comp (op, stack) incomeOp
  | (topOp == "*" || topOp == "/") && (incomeOp == "+" || incomeOp == "-") = comp (drop 1 op, (stack ++ [topOp])) incomeOp
  | (topOp == "+" || topOp == "-") && (incomeOp == "*" || incomeOp == "/") = (incomeOp : op, stack)
  | otherwise = (incomeOp : op, stack)
  where topOp = head op




