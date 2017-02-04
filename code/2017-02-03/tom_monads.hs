-- Custom implementations of maybe, either, and list, and their instances of functor, applicative and monad

import Control.Applicative

data MyMaybe a = Justy a | Nun

instance Functor MyMaybe where
  fmap f (Justy x) = Justy $ f x
  fmap f Nun = Nun

instance Applicative MyMaybe where
  pure = Justy
  Nun <*> _ = Nun
  _ <*> Nun = Nun
  (Justy f) <*> (Justy x) = Justy $ f x

instance Monad MyMaybe where
  return = pure
  Nun >>= _ = Nun
  (Justy a) >>= f = f a

data MyEither l r = MyLeft l | MyRight r deriving (Show)

instance Functor (MyEither l) where
   fmap f (MyLeft l) = MyLeft l
   fmap f (MyRight x) = MyRight (f x)

instance Applicative (MyEither l) where
   pure x = MyRight x
   (MyLeft l) <*> _ = MyLeft l
   _ <*> (MyLeft l) = MyLeft l
   (MyRight f) <*> (MyRight x) = MyRight (f x)

instance Monad (MyEither l) where
  return = pure
  (MyLeft x) >>= _ = (MyLeft x)
  (MyRight x) >>= f = f x

data MyList a = Empty | Cons a (MyList a) deriving Show

instance Functor MyList where
  fmap f Empty = Empty
  fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative MyList where
  pure x = Cons x Empty
  Empty <*> _ = Empty
  _ <*> Empty = Empty
  (Cons f fs) <*> xs = run xs where
     run Empty = fs <*> xs
     run (Cons y ys) = Cons (f y) (run ys)

myConcat :: MyList a -> MyList a -> MyList a
myConcat Empty x = x
myConcat x Empty = x
myConcat (Cons x xs) ys = Cons x (myConcat xs ys)

instance Monad MyList where
  return = pure
  Empty >>= _ = Empty
  Cons x xs >>= f = myConcat (f x) (xs >>= f)

my :: [a] -> MyList a
my [] = Empty
my (x:xs) = Cons x (my xs)
