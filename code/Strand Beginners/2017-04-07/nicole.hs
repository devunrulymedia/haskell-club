fizzbuzz range =
  [if x `mod` 15 == 0 then "FIZZBUZZ"
    else if x `mod` 3 == 0 then "FIZZ"
    else if x `mod` 5 == 0 then "BUZZ"
      else (show x) | x <- range]
