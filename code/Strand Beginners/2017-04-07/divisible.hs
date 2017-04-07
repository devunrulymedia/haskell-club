divisible_by num divisor = num `mod` divisor == 0

-- [x | x <- [1..20], x `divisible_by` 3]

-- [x | x <- [1..20], x `mod` 3 == 0]
-- [x | x <- [1..20], x `mod` 5 == 0]
-- [x | x <- [1..20], x `mod` 15 == 0]
-- expression | provide values for expression parameters,
-- [x ++ y | x <- ["Hello", "Goodbye"], y <- ["World", "Chicago"]]


-- [if x `mod` 3 == 0 then "FIZZ" else (show x) | x <- [1..20]]
