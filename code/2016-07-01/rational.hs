import Control.Monad (liftM2)
import Test.QuickCheck


data Rational' = Rational' Int Int


instance Num Rational' where 
    Rational' a b * Rational' c d = lowestTerms $ Rational' (a * c) (b * d)
    Rational' a b + Rational' c d = lowestTerms $ Rational' ((a * d) + (c * b))  (b * d)
    abs (Rational' a b)           = Rational' (abs a) (abs b)
    signum (Rational' a b)        = Rational' ((signum a) * (signum b)) 1
    negate (Rational' a b)        = Rational' (negate a) b
    fromInteger a                 = Rational' (fromIntegral a) 1
    

lowestTerms :: Rational' -> Rational'
lowestTerms (Rational' 0 0) = Rational' 0 0
lowestTerms (Rational' a b) = Rational' numerator denominator
    where greatestCommonDivisor = gcd a b
          numerator = a `div` greatestCommonDivisor
          denominator = b `div` greatestCommonDivisor


instance Fractional Rational' where
--  (/) (Rational' a b) (Rational' c d) = (Rational' a b) * (Rational' d c)
    recip (Rational' a b) = (Rational' b a) 


instance Show Rational' where
    show (Rational' a b) = show a ++ "/" ++ show b


instance Eq Rational' where
    Rational' a b == Rational' c d = (a==c) && (b==d)


-- Tests

instance Arbitrary Rational' where
    arbitrary = liftM2 Rational' arbitrary arbitrary


prop_add :: Rational' -> Rational' -> Property
prop_add a b = a+b === b+a
