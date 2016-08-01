import Data.List
import Data.Ord
import GHC.Exts
import Control.Monad
import qualified Data.Map

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

-- builds a bag/multiset of key:item to value:count
bagof:: (Ord a) => [a] -> Data.Map.Map a Int
bagof [] = Data.Map.empty
bagof (x:xs) = Data.Map.insertWith (+) x 1 (bagof xs)

bagof2:: (Ord a) => [a] -> Data.Map.Map a Int
bagof2 = foldl count Data.Map.empty where count m x = Data.Map.insertWith (+) x 1 m

-- frequency function for a list which precalculates a bag, so we only need to walk the list once
freqc:: (Ord a) => [a] -> a -> Int
freqc xs = let m = bagof xs
           in \x -> Data.Map.findWithDefault 0 x m

-- requires an Ord a because of the precalculated bag, which uses map which requires Ord a
freqsort2:: (Ord a) => [[a]] -> [[a]]
freqsort2 xs = let xs' = [(x, length x) | x <- xs] 
                   lfreqc = freqc (map snd xs') 
                   xs'' = [(x, l, lfreqc l) | (x, l) <- xs'] 
                   sorted = sortWith (\(_, _, f) -> f) xs''
               in [x | (x, _, _) <- sorted]    