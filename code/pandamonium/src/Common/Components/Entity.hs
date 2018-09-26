module Common.Components.Entity where

import Data.Dynamic
import Data.Maybe

data Entity = Entity [ Dynamic ]

data EntityId = EntityId Int deriving Eq

instance Enum EntityId where
  toEnum = EntityId
  fromEnum (EntityId entityId) = entityId

entity :: Entity
entity = Entity []

infixl 4 <-+

(<-+) :: (Typeable a) => Entity -> a -> Entity
(<-+) (Entity xs) a = Entity (replace xs a)

replace :: Typeable a => [ Dynamic ] -> a -> [ Dynamic ]
replace [] a = [toDyn a]
replace (x:xs) a = if typesMatch (fromDynamic x) a
  then toDyn a : xs
  else x : replace xs a where
    typesMatch :: Maybe a -> a -> Bool
    typesMatch (Just _) _ = True
    typesMatch Nothing  _ = False

from :: (Typeable a) => Entity -> Maybe a
from (Entity xs) = from' xs where
  from' [] = Nothing
  from' (x : xs) = case (fromDynamic x) of
    (Just a) -> Just a
    Nothing -> from' xs

apply1 :: (Typeable a) => (a -> b) -> Entity -> Maybe b
apply1 f c = pure f <*> from c

apply2 :: (Typeable a, Typeable b) => (a -> b -> c) -> Entity -> Maybe c
apply2 f c = pure f <*> from c <*> from c

apply3 :: (Typeable a, Typeable b, Typeable c) => (a -> b -> c -> d) -> Entity -> Maybe d
apply3 f c = pure f <*> from c <*> from c <*> from c

update :: (Typeable a, Typeable b, Typeable c)
       => (Float -> a -> b -> c) -> Float -> Entity -> Entity
update f t c = fromMaybe c $ do
  a <- from c
  b <- from c
  return $ c <-+ f t a b
