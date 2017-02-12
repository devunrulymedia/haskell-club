import Control.Applicative

-- verify the Applicative Functor laws
-- for Maybe, which is an instance of Control.Applicative 

-- identity
-- pure id <*> v = v
   
Just id <*> Just "banana"
Just "banana"



-- composition
-- pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
  
   

let f = (*10)
let g = \x -> x - 1

f (g 2)
(.) f g 2

Just f <*> (Just g <*> Just 2)
Just (.) <*> Just f <*> Just g <*> Just 2

-- homomorphism
-- pure f <*> pure x = pure (f x)

let g = \x -> x - 1

Just g <*> Just 3
Just (g 3)

-- interchange
-- u <*> pure y = pure ($ y) <*> u

let g = \x -> x - 1

g 5
($ 5) g

Just g <*> Just 5
Just ($ 5) <*> Just g



