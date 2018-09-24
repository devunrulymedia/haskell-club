module Common.Components (Components, components, (<-+), from, apply1, apply2) where

import Data.Dynamic

data Components = Components [ Dynamic ]

components :: Components
components = Components []

infixl 4 <-+

(<-+) :: (Typeable a) => Components -> a -> Components
(<-+) = put

replace :: Typeable a => [ Dynamic ] -> a -> [ Dynamic ]
replace [] a = [toDyn a]
replace (x:xs) a = if typesMatch (fromDynamic x) a
  then toDyn a : xs
  else x : replace xs a where
    typesMatch :: Maybe a -> a -> Bool
    typesMatch (Just _) _ = True
    typesMatch Nothing  _ = False

put :: (Typeable a) => Components -> a -> Components
put (Components xs) a = Components (replace xs a)

from :: (Typeable a) => Components -> Maybe a
from (Components xs) = from' xs where
  from' [] = Nothing
  from' (x : xs) = case (fromDynamic x) of
    (Just a) -> Just a
    Nothing -> from' xs

apply1 :: (Typeable a) => (a -> b) -> Components -> Maybe b
apply1 f c = pure f <*> from c

apply2 :: (Typeable a, Typeable b) => (a -> b -> c) -> Components -> Maybe c
apply2 f c = pure f <*> from c <*> from c
