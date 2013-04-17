-- Monday 15 April
data NonEmptyList a = Singleton a | Cons a (NonEmptyList a) deriving Show

head' :: NonEmptyList a -> Bool
head' a = True


-- data ListWithLength a i =
--   ;a 0 = C

data ListN0 a = ListN0 deriving (Show)
data ListN1 a = ListN1 a (ListN0 a) deriving (Show)
data ListN2 a = ListN2 a (ListN1 a) deriving (Show)
data ListN3 a = ListN3 a (ListN2 a) deriving (Show)
data ListN4 a = ListN4 a (ListN3 a) deriving (Show)
data ListN5 a = ListN5 a (ListN4 a) deriving (Show)
data ListN6 a = ListN6 a (ListN5 a) deriving (Show)
data ListN7 a = ListN7 a (ListN6 a) deriving (Show) 
data ListN8 a = ListN8 a (ListN7 a) deriving (Show)
