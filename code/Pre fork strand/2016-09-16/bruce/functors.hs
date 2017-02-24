import Control.Applicative

data Tuple a = Tuple a a deriving (Show)

instance Functor Tuple where
	fmap f (Tuple x y) = Tuple (f x) (f y)


data HetroTuple a b = HetroTuple a b deriving (Show)

data HetroTuple' f a b = HetroTuple' { left :: b, right :: f a} deriving (Show)

instance Functor (HetroTuple a) where
	fmap f (HetroTuple x y) = HetroTuple x (f y) 

instance Functor (HetroTuple' a b) where
	fmap f (HetroTuple' {left = x, right = y}) = HetroTuple' {left = f x, right = y}

-- (<*>) :: f (a -> b) -> f a -> f b  

instance Applicative Tuple where
	pure x = Tuple x x
	(<*>) (Tuple f g) (Tuple x y) = Tuple (f x) (g y)
