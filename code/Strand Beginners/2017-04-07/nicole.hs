divisible_by num divisor = num `mod` divisor == 0

fizzbuzz range =
  [if x `divisible_by` 15 then "FIZZBUZZ"
    else if x `divisible_by` 3 then "FIZZ"
    else if x `divisible_by` 5 then "BUZZ"
      else (show x) | x <- range]
