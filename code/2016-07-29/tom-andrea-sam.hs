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

freqSort:: [[a]] -> [[a]]
freqSort xs = join (groupBy (equal length) (sortWith length xs))
                where equal f a b = (f a) == (f b)