module Common.Components where

import Data.Dynamic

data Components = Components [ Dynamic ]

components :: Components
components = Components []

infixl 4 <-+

(<-+) :: (Typeable a) => Components -> a -> Components
(<-+) = put

put :: (Typeable a) => Components -> a -> Components
put (Components xs) a = Components $ toDyn a : xs

from :: (Typeable a) => Components -> Maybe a
from (Components xs) = from' xs where
  from' [] = Nothing
  from' (x : xs) = case (fromDynamic x) of
    (Just a) -> Just a
    Nothing -> from' xs
