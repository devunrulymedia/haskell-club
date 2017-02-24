data Rational' = Rational' Int Int deriving (Show)

instance Num Rational' where 
        Rational' a b * Rational' c d = Rational' (a * c) (b * d)
        Rational' a b + Rational' c d = Rational' ((a * d) + (c * b))  (b * d)

