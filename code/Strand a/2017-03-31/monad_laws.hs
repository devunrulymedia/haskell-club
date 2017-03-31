
import Control.Monad

data Broken a = Broken a Int deriving (Show)

instance Monad Broken where
  return x = Broken x 0
  (>>=) (Broken x n) f = f x
