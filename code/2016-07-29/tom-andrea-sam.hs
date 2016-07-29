import Data.List
import Data.Ord
import GHC.Exts
import Control.Monad

compareLengths a b
  | lenA < lenB  = LT
  | lenA == lenB = EQ
  | lenA > lenB  = GT
    where lenA = length a
          lenB = length b

compareLengths2 a b = compare (length a) (length b)

lsort:: [[a]] -> [[a]]
lsort = (Data.List.sortBy compareLengths2) 

lsort2:: [[a]] -> [[a]]
lsort2 = sortBy (\a b -> compare (length a) (length b))

lsort3:: [[a]] -> [[a]]
lsort3 = sortWith length 



unstableFreqSort:: [[a]] -> [[a]]
unstableFreqSort xs = join (groupBy (equal length) (sortWith length xs))
                       where equal f a b = (f a) == (f b)

addToGroup:: (Eq p) => [([a], p)] -> (a, p) -> [([a], p)]
addToGroup [] (x, v) = [([x], v)]
addToGroup ((xs, p):rest) (x, v) 
  | p == v = ((xs ++ [x]), p):rest
  | p /= v = (xs, p):(addToGroup rest (x, v))

accumulate:: (Eq p) => (a -> p) -> [a] -> [([a], p)]
accumulate = accum' [] where 
                accum' acc f [] = acc
                accum' acc f (x:xs) = accum' (addToGroup acc (x, (f x))) f xs

freqSort:: [[a]] -> [[a]]
freqSort xs = join(map fst (sortWith (length . fst) (accumulate length xs)))
