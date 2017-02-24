
-- Raw pattern matching: avoids need to concatenate
dropEveryNth :: Int -> [a] -> [a]
dropEveryNth n xs = dropEveryNth' 1 xs where
   dropEveryNth' _ [] = []
   dropEveryNth' c (x:xs) 
      | n == c = dropEveryNth' 1 xs
      | otherwise = x : (dropEveryNth' (c + 1) xs)

-- Using take and drop: cleaner but concat can be slow
dropEvery :: Int -> [a] -> [a]
dropEvery _ [] = []
dropEvery n xs = take (n-1) xs ++ dropEvery n (drop n xs)

dropEveryThird :: [a] -> [a]
dropEveryThird = dropEveryNth 3

-- Rewriting library functions!
myLength :: [a] -> Int
myLength [] = 0
myLength (x:xs) = myLength xs + 1

myTake :: Int -> [a] -> [a]
myTake _ [] = []
myTake 0 xs = []
myTake n (x:xs) = x : (myTake (n-1) xs)

myReverse :: [a] -> [a]
myReverse xs = myReverse' xs [] where
    myReverse' [] acc = acc
    myReverse' (x:xs) acc = myReverse' xs (x:acc)

myDrop :: Int -> [a] -> [a]
myDrop _ [] = []
myDrop 0 xs = xs
myDrop n (x:xs) = myDrop (n-1) xs
