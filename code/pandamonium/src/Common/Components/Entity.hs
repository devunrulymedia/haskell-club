{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Strict #-}

module Common.Components.Entity where

import Data.Dynamic (Typeable)
import Data.ConstrainedDynamic
import Data.Maybe

class Typeable a => Component a

type DynComp = ConstrainedDynamic Component

type EntityId = Int

data Entity = Entity EntityId [ DynComp ]

data Owner = Owner EntityId deriving Component

class View a where
  entityFrom :: a -> Entity

entity :: EntityId -> Entity
entity id = Entity id []

entityId :: Entity -> EntityId
entityId (Entity x _) = x

infixl 4 <-+
infixl 4 <-:
infixl 4 <-|

typesMatch :: Maybe a -> a -> Bool
typesMatch (Just _) _ = True
typesMatch Nothing  _ = False

(<-+) :: (Component a) => Entity -> a -> Entity
(<-+) (Entity x cs) a = Entity x (replace cs a) where
  replace :: Component a => [ DynComp ] -> a -> [ DynComp ]
  replace [] a = [toDyn a]
  replace (c:cs) a = if typesMatch (fromDynamic c) a
    then toDyn a : cs
    else c : replace cs a

(<-:) :: Component a => (EntityId -> Entity) -> a -> (EntityId -> Entity)
(<-:) f a = (<-+ a) . f

(<-|) :: Component a => Entity -> (a -> a) -> Entity
(<-|) = flip update

extract :: (Component a) => Entity -> Maybe a
extract (Entity x cs) = extract' cs where
  extract' [] = Nothing
  extract' (c : cs) = case (fromDynamic c) of
    (Just a) -> Just a
    Nothing -> extract' cs

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
        => (t -> a -> b) -> t -> Entity -> Entity
update1 f t c = fromMaybe c $ do
  a <- extract c
  return $ c <-+ f t a

update2 :: (Component a, Component b, Component c)
        => (t -> a -> b -> c) -> t -> Entity -> Entity
update2 f t c = fromMaybe c $ do
  a <- extract c
  b <- extract c
  return $ c <-+ f t a b

updateM1 :: (Component a, Component b, Monad m)
         => (t -> a -> m b) -> t -> Entity -> m Entity
updateM1 f t e = fromMaybe (return e) $ do
  a <- extract e
  return (do a' <- f t a; return (e <-+ a'))

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
