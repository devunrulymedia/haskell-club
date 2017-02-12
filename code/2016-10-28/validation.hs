import Control.Applicative

newtype Validation a = Validation (Either String a)


foo :: (Integral a) => Validation a -> a
foo (Validation (Right a)) = a + 1

newtype ZapList a = ZapList [a] deriving Show

instance Functor ZapList where
  fmap f (ZapList xs) = ZapList (map f xs)

instance Applicative ZapList where
  pure a = ZapList (repeat a)
  ZapList fs <*> ZapList xs = ZapList (zipWith (\f x -> f x) fs xs)  
