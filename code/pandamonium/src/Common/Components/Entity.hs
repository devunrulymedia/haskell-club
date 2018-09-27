{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Components.Entity where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Data.Maybe

class Typeable a => Component a where
  uniqueComponent :: a -> Bool
  uniqueComponent a = True

type DynComp = ConstrainedDynamic Component

data Entity = Entity [ DynComp ]

data EntityId = EntityId Int deriving (Eq, Component)

instance Enum EntityId where
  toEnum = EntityId
  fromEnum (EntityId entityId) = entityId

entity :: Entity
entity = Entity []

infixl 4 <-+

(<-+) :: (Component a) => Entity -> a -> Entity
(<-+) (Entity xs) a = if uniqueComponent a
  then Entity (replace xs a)
  else Entity (toDyn a : xs)

replace :: Component a => [ DynComp ] -> a -> [ DynComp ]
replace [] a = [toDyn a]
replace (x:xs) a = if typesMatch (fromDynamic x) a
  then toDyn a : xs
  else x : replace xs a where
    typesMatch :: Maybe a -> a -> Bool
    typesMatch (Just _) _ = True
    typesMatch Nothing  _ = False

from :: (Component a) => Entity -> Maybe a
from (Entity xs) = from' xs where
  from' [] = Nothing
  from' (x : xs) = case (fromDynamic x) of
    (Just a) -> Just a
    Nothing -> from' xs

consumeAll :: forall a . (Component a) => Entity -> (Entity, [a])
consumeAll (Entity xs) =
  let (xs', as) = consumeAll' xs ([], [])
   in (Entity xs', as) where
      consumeAll' :: [DynComp] -> ([DynComp], [a]) -> ([DynComp], [a])
      consumeAll' [] (xs', as) = (xs', as)
      consumeAll' (x:xs) (xs', as) = case (fromDynamic x) of
        Nothing  -> consumeAll' xs (x:xs', as)
        (Just a) -> consumeAll' xs (xs', a:as)

allFrom :: (Component a) => Entity -> [a]
allFrom (Entity xs) = allFrom' xs where
  allFrom' [] = []
  allFrom' (x : xs) = case (fromDynamic x) of
    (Just a) -> a : allFrom' xs
    Nothing  -> allFrom' xs

apply1 :: (Component a) => (a -> b) -> Entity -> Maybe b
apply1 f c = pure f <*> from c

apply2 :: (Component a, Component b) => (a -> b -> c) -> Entity -> Maybe c
apply2 f c = pure f <*> from c <*> from c

apply3 :: (Component a, Component b, Component c) => (a -> b -> c -> d) -> Entity -> Maybe d
apply3 f c = pure f <*> from c <*> from c <*> from c

update :: (Component a, Component b, Component c)
       => (Float -> a -> b -> c) -> Float -> Entity -> Entity
update f t c = fromMaybe c $ do
  a <- from c
  b <- from c
  return $ c <-+ f t a b
