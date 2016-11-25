import Data.List

data Solution = Val Rational | Sum Solution Solution | Diff Solution Solution | Mult Solution Solution | Div Solution Solution deriving (Show)

eval :: Solution -> Rational
eval (Val a) = a
eval (Sum a b) = eval a + eval b
eval (Diff a b) = eval a - eval b		
eval (Mult a b) = eval a * eval b
eval (Div a b) = eval a / eval b

solutions :: Solution -> Solution -> [Solution]
solutions x y = [x, 
                 y, 
                 Sum x y, 
                 Mult x y, 
                 Diff x y, 
                 Diff y x, 
                 Div x y, 
                 Div y x]

solves :: Rational -> Solution -> Bool
solves t s = eval s == t

solve :: Rational -> Rational -> Rational -> [Solution]
solve t v1 v2 = filter (solves t) (solutions (Val v1) (Val v2))

allSolutions :: [Solution] -> [Solution]
allSolutions [] = []
allSolutions [x] = [x]
allSolutions (x:xs) = concat (map (solutions x) (allSolutions xs))

solve'' :: Rational -> [Solution] -> [Solution]
solve'' t xs = filter (solves t) (concat $ map allSolutions (permutations xs))

solve' :: Rational -> [Rational] -> [Solution]
solve' t vs = solve'' t (map Val vs)
