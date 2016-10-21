newtype Validation a = Validation { getValidation :: Either String a } deriving (Show)

instance Functor Validation where
  fmap f (Validation (Left x)) = Validation (Left x)
  fmap f (Validation (Right a)) = Validation (Right (f a))

instance Applicative Validation where
  pure = Validation . Right
  (Validation (Left x))  <*> _ = Validation (Left x)
  (Validation (Right f)) <*> v = fmap f v
