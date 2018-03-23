head' [] = error "empty"
head' (a:_) = a

tail' [] = error "empty"
tail' (_:as) = as

odd' :: Int -> Bool
odd' 0 = False
odd' x = even' (x - 1)

even' :: Int -> Bool
even' 0 = True
even' x = odd' (x - 1)

fact :: Integer -> Integer
fact 1 = 1
fact n = n * (fact (n - 1))

