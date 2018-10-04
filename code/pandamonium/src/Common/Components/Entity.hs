{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Common.Components.Entity where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Data.Maybe

class Typeable a => Component a

type DynComp = ConstrainedDynamic Component

data Entity = Entity [ DynComp ]

data EntityId = EntityId Int deriving (Eq, Component)

instance Enum EntityId where
  toEnum = EntityId
  fromEnum (EntityId entityId) = entityId

class View a where
  entityFrom :: a -> Entity

entity :: Entity
entity = Entity []

infixl 4 <-+
infixl 4 <-|

typesMatch :: Maybe a -> a -> Bool
typesMatch (Just _) _ = True
typesMatch Nothing  _ = False

(<-+) :: (Component a) => Entity -> a -> Entity
(<-+) (Entity xs) a = Entity (replace xs a) where
  replace :: Component a => [ DynComp ] -> a -> [ DynComp ]
  replace [] a = [toDyn a]
  replace (x:xs) a = if typesMatch (fromDynamic x) a
    then toDyn a : xs
    else x : replace xs a

(<-|) :: Component a => Entity -> (a -> a) -> Entity
(<-|) = flip update

extract :: (Component a) => Entity -> Maybe a
extract (Entity xs) = extract' xs where
  extract' [] = Nothing
  extract' (x : xs) = case (fromDynamic x) of
    (Just a) -> Just a
    Nothing -> extract' xs

extractOr :: (Component a) => a -> Entity -> a
extractOr a e = fromMaybe a (extract e)

update :: (Component a) => (a -> a) -> Entity -> Entity
update f e = case (extract e) of
  (Just a) -> e <-+ f a
  Nothing  -> e

apply1 :: (Component a) => (a -> b) -> Entity -> Maybe b
apply1 f c = pure f <*> extract c

apply2 :: (Component a, Component b) => (a -> b -> c) -> Entity -> Maybe c
apply2 f c = pure f <*> extract c <*> extract c

apply3 :: (Component a, Component b, Component c) => (a -> b -> c -> d) -> Entity -> Maybe d
apply3 f c = pure f <*> extract c <*> extract c <*> extract c

update1 :: (Component a, Component b)
        => (Float -> a -> b) -> Float -> Entity -> Entity
update1 f t c = fromMaybe c $ do
  a <- extract c
  return $ c <-+ f t a


update2 :: (Component a, Component b, Component c)
        => (Float -> a -> b -> c) -> Float -> Entity -> Entity
update2 f t c = fromMaybe c $ do
  a <- extract c
  b <- extract c
  return $ c <-+ f t a b

applyM :: (Component a, Monad m)
       => (Float -> a -> m ()) -> Float -> Entity -> m Entity
applyM f t e = fromMaybe (return e) $ do
  a <- extract e
  return $ (do f t a; return e)

applyM2 :: (Component a, Component b, Monad m)
        => (Float -> a -> b -> m ()) -> Float -> Entity -> m Entity
applyM2 f t e = fromMaybe (return e) $ do
  a <- extract e; b <- extract e
  return $ (do f t a b; return e)

applyM3 :: (Component a, Component b, Component c, Monad m)
        => (Float -> a -> b -> c -> m ()) -> Float -> Entity -> m Entity
applyM3 f t e = fromMaybe (return e) $ do
  a <- extract e; b <- extract e; c <- extract e
  return $ (do f t a b c; return e)
