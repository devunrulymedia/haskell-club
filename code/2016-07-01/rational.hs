data Rational' = Rational' Int Int


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


instance Show Rational' where
    show (Rational' a b) = show a ++ "/" ++ show b
