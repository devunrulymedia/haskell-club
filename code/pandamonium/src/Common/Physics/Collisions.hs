{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Physics.Collisions where

import Graphics.Gloss.Data.Vector
import Control.Lens
import Data.Dynamic

import Common.Redux
import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Shapes.Shape

data ResetCollisions = ResetCollisions deriving (Show, ReduxEvent)
data Collision t i = Collision t i t i Vector deriving (Show, ReduxEvent)

touch :: (Shaped a, Shaped b, Typeable t, Show t, Typeable i, Show i)
  => Entity t i a -> Entity t i b -> Events ()
touch a b = case (shape a !!> shape b) of
  Nothing -> return ()
  (Just pushout) -> do fireEvent (Collision (a ^. etype) (a ^. eid) (b ^. etype) (b ^. eid) (negate pushout))
                       fireEvent (Collision (b ^. etype) (b ^. eid) (a ^. etype) (a ^. eid) pushout)

bounce_against_static :: (Moving a, Shaped a, Shaped b, Typeable t, Show t, Typeable i, Show i)
  => Float -> Entity t i a -> Entity t i b -> Events (Entity t i a, Entity t i b)
bounce_against_static el a b = case (shape b !!> shape a) of
  Nothing -> return (a, b)
  (Just pushout) -> do
    fireEvent (Collision (a ^. etype) (a ^. eid) (b ^. etype) (b ^. eid) offset)
    fireEvent (Collision (b ^. etype) (b ^. eid) (a ^. etype) (a ^. eid) (0, 0))
    return (move offset (applyImpulse reflected_vel a), b) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push
