module Common.Components (components, (<-+), from) where

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
