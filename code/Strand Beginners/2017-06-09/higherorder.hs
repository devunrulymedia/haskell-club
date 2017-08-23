collatz :: (Integral a) => a -> [a]
collatz 1 = [1]
collatz n 
  | even n = n : collatz(n `div` 2)
  | odd n = n : collatz(3 * n + 1)


