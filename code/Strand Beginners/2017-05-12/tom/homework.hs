data RationalNumber = RationalNumber Integer Integer

lcd :: Integer ->  Integer -> Integer
lcd x 0 = x
lcd x y 
  | y > x = lcd y x
  | otherwise = lcd y (rem x y)

simplify :: RationalNumber -> RationalNumber
simplify (RationalNumber num den) = RationalNumber (num `quot` lcd') (den `quot` lcd') where
   lcd' = lcd num den

instance Show RationalNumber where
  show (RationalNumber num 1) = show num
  show x = show num ++ "/" ++ show den where
    (RationalNumber num den) = simplify x

instance Num RationalNumber where
  (RationalNumber n1 d1) + (RationalNumber n2 d2) = simplify $ RationalNumber (n1*d2 + n2*d1) (d1*d2)
  (RationalNumber n1 d1) * (RationalNumber n2 d2) = simplify $ RationalNumber (n1*n2) (d1*d2)
  negate (RationalNumber n1 d1) = simplify $ RationalNumber (-n1) d1
  abs (RationalNumber n1 d1) = RationalNumber (abs n1) (abs d1)
  fromInteger n = RationalNumber n 1
  signum (RationalNumber n1 d1) = RationalNumber ((signum n1) * (signum d1)) 1
