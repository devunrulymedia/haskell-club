--foo, can you read this
solvePuzzle :: ([Int],Int) -> String 
solvePuzzle ([x],y) | 
  x == y = show x ++ " = " ++ show x

solvePuzzle ([a,b],y)
  | a <= 0 || b <= 0 = error "Nope"
  | a + b == y = showAnswer "+"
  | a * b == y = showAnswer "*"
  | a - b == y = showAnswer "-"
  | a `div` b == y = showAnswer "/"
  | b `div` a == y = solvePuzzle ([b, a], y)
  | b - a == y = solvePuzzle ([b, a], y)
  where showAnswer (arg) = show a ++ arg ++ show b ++ " = "  ++ show y