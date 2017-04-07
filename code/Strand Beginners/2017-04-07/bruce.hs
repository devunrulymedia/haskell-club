
fizzbuzz :: Int -> Int -> [String]
fizzbuzz n m = [ fb x | x <- [n..m]]

fb :: Int -> String
fb x   
	| multipleOf 3 && multipleOf 5 = fizz ++ buzz
	| multipleOf 3 = fizz
	| multipleOf 5 = buzz
	| otherwise = show(x)
	where multipleOf n = x `mod` n == 0
	      fizz = "fizz"
	      buzz = "buzz"	

