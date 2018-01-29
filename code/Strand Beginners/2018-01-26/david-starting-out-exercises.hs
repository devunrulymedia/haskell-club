penultimate l = last (init l)

findK k l = l !! k

isPalindrome l = l == reverse l

duplicate xs = concat [ [x, x] | x <- xs ]

ziplike xs ys = [ (xs !! i, ys !! i) | i <- [0 .. min (length xs) (length ys) - 1]]

splitAtIndex k l = (take k l, drop k l)

dropK k l = take k l ++ drop (k +1) l

slice i k l = take (k-i) (drop i l)

insertElem x k l = take k l ++ x:(drop k l)

rotate n l = drop n l ++ take n l