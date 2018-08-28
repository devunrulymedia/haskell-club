{-# LANGUAGE MultiParamTypeClasses #-}

module Common.Physics.Collisions where

import Graphics.Gloss.Data.Vector
import Control.Lens

import Common.Redux
import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Shapes.Shape

class CollisionEvent t i e where
  collisionEvent :: t -> i -> t -> i -> Vector -> e

bounce_against_static :: (Moving a, Shaped a, Shaped b, CollisionEvent t i e) => Float -> Entity t i a -> Entity t i b -> Events e (Entity t i a)
bounce_against_static el a b = case (shape b !!> shape a) of
  Nothing -> return a
  (Just pushout) -> do
    fireEvent (collisionEvent (a ^. etype) (a ^. eid) (b ^. etype) (b ^. eid) offset)
    fireEvent (collisionEvent (b ^. etype) (b ^. eid) (a ^. etype) (a ^. eid) (0, 0))
    return (move offset (applyImpulse reflected_vel a)) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push
