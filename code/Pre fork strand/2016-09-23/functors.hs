import Control.Applicative

data Tuple a = Tuple a a deriving (Show)

instance Functor Tuple where
	fmap f (Tuple x y) = Tuple (f x) (f y)

-- (<*>) :: f (a -> b) -> f a -> f b  

instance Applicative Tuple where
	pure x = Tuple x x
	(<*>) (Tuple f g) (Tuple x y) = Tuple (f x) (g y)


data MyEither l r = MyLeft l | MyRight r deriving (Show)

instance Functor (MyEither l) where
   fmap f (MyLeft l) = MyLeft l
   fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither l) where
   pure x = MyRight x
   (<*>) (MyLeft l) _ = MyLeft l
   (<*>) _ (MyLeft l) = MyLeft l
   (<*>) (MyRight f) (MyRight x) = MyRight (f x) 
