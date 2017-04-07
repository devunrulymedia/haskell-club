fizz :: Int -> String
fizz x = if x `mod` 3 == 0 then "Fizz" else ""

buzz :: Int -> String
buzz x = if x `mod` 5 == 0 then "Buzz" else ""

fibu :: Int -> String
fibu x = if fizz(x) ++ buzz(x) == "" then show x else fizz(x) ++ buzz(x)

fizzbuzz :: [Int] -> [String]
fizzbuzz xs = map fibu xs 
