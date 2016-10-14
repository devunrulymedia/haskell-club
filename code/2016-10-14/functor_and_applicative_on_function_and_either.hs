infixr 1 <**>

-- these type classes are exactly the same as Functor and Applicative
-- so they don't clash with the standard implementations which ghci brings in

class Functory f where
    fmapy :: (a -> b) -> f a -> f b

class (Functory f) => Applicativey f where
    purey :: a -> f a
    (<**>) :: f (a -> b) -> f a -> f b


instance Functory (Either l) where
    fmapy f (Right x) = Right $ f x
    fmapy f (Left x) = Left x

instance Applicativey (Either l) where
    purey = Right
    (Left x) <**> _ = Left x
    (Right f) <**> x = fmapy f x

-- this is a great example of the only possible solution being the one that typechecks

instance Functory ((->) r) where
    -- fmapy :: (a -> b) -> (r -> a) -> r -> b
    -- we want a b, which means we need to pass an a into f
    -- so we want an a, which we can get from passing x into f'
    fmapy f f' x = f (f' x)

instance Applicativey ((->) r) where
    -- substituting into the typeclass: pure :: a -> r -> a
    -- here, we provide an a, and we want a function which goes from r to a back
    -- only possible answer is to return the a that was provided
    purey = const
    -- substituting into the typeclass: <**> :: (r -> a -> b) -> (r -> a) -> r -> b
    -- here, given an (r -> a -> b), an (r -> a) and an r, we want a b.
    -- only way we can get a b is by passing an r and an a to f
    -- only way we can get an a is by passing an r to g
    -- only r we have is x
    (<**>) f g x = f x $ g x

