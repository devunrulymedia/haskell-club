import GHC.Exts

lsort :: [[a]] -> [[a]]
lsort a = sortWith (\a -> length a) a