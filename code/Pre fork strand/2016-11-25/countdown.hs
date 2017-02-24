import Data.List

data Solution = Val Integer | Sum Solution Solution | Diff Solution Solution | Mult Solution Solution | Div Solution Solution

instance Show Solution where
  show (Val x) = show x
  show (Sum x y) = "(" ++ (show x) ++ "+" ++ (show y) ++ ")"
  show (Mult x y) = "(" ++ (show x) ++ "*" ++ (show y) ++ ")"
  show (Diff x y) = "(" ++ (show x) ++ "-" ++ (show y) ++ ")"
  show (Div x y) = "(" ++ (show x) ++ "/" ++ (show y) ++ ")"

instance Num Solution where
  (+) = Sum 
  (-) = Diff
  (*) = Mult
  fromInteger = Val

eval :: Solution -> Integer
eval (Val a) = a
eval (Sum a b) = eval a + eval b
eval (Diff a b) = eval a - eval b		
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a `quot` eval b

solutions :: Solution -> Solution -> [Solution]
solutions x y 
  | eval x == 0 && eval y == 0 = []
  | eval x == 0 = [y]
  | eval y == 0 = [x]
  | rem (eval x) (eval y) == 0 = (Div x y) : usuals
  | rem (eval y) (eval x) == 0 = (Div y x) : usuals
  | otherwise = usuals
  where usuals = [x, y, Sum x y, Mult x y, Diff x y, Diff y x]

solves :: Integer -> Solution -> Bool
solves t s = eval s == t

allSolutions :: [Solution] -> [Solution]
allSolutions [] = []
allSolutions [x] = [x]
allSolutions (x:xs) = concat (map (solutions x) (allSolutions xs))

solve' :: Integer -> [Solution] -> [Solution]
solve' t xs = filter (solves t) (concat $ map allSolutions (permutations xs))

solve :: Integer -> [Integer] -> [Solution]
solve t vs = solve' t (map Val vs)
