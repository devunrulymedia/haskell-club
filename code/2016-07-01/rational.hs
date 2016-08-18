data Rational' = Rational' Int Int deriving (Show)


instance Num Rational' where 
    Rational' a b * Rational' c d = Rational' (a * c) (b * d)
    Rational' a b + Rational' c d = Rational' ((a * d) + (c * b))  (b * d)
    abs (Rational' a b) = Rational' (abs a) (abs b)
    signum (Rational' a b) = Rational' ((signum a) * (signum b)) 1
    negate (Rational' a b) = Rational' (negate a) b
    fromInteger a = Rational' (fromIntegral a) 1
    

instance Fractional Rational' where
--  (/) (Rational' a b) (Rational' c d) = (Rational' a b) * (Rational' d c)
    recip (Rational' a b) = (Rational' b a) 
