{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE DeriveAnyClass #-}

module Common.Physics.Collisions where

import Graphics.Gloss.Data.Vector
import Control.Lens
import Data.Dynamic

import Common.Redux
import Common.Entities.Entity
import Common.Entities.TypeClasses.Shapes
import Common.Entities.TypeClasses.Physics
import Common.Shapes.Shape
import Common.Physics.Physics

data ResetCollisions = ResetCollisions deriving (Show, ReduxEvent)
data Collision t i = Collision t i t i Vector deriving (Show, ReduxEvent)

fireCollision :: (Typeable t, Show t, Typeable i, Show i)
  => Entity t i a -> Entity t i b -> Vector -> Events ()
fireCollision a b v = fireEvent (Collision (a ^. etype) (a ^. eid) (b ^. etype) (b ^. eid) v)

touch :: (Shaped a, Shaped b, Typeable t, Show t, Typeable i, Show i)
  => Entity t i a -> Entity t i b -> Events ()
touch a b = case (shape a !!> shape b) of
  Nothing -> return ()
  (Just pushout) -> do fireCollision a b (negate pushout)
                       fireCollision b a pushout

bounce_against_static :: (Moving a, Shaped a, Shaped b, Typeable t, Show t, Typeable i, Show i)
  => Float -> Entity t i a -> Entity t i b -> Events (Entity t i a, Entity t i b)
bounce_against_static el a b = case (shape b !!> shape a) of
  Nothing -> return (a, b)
  (Just pushout) -> do
    fireCollision a b offset
    fireCollision b a (0, 0)
    return (move offset (applyImpulse reflected_vel a), b) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push

bounce :: (Physics a, Physics b, Typeable t, Show t, Typeable i, Show i)
  => Entity t i a -> Entity t i b -> Events (Entity t i a, Entity t i b)
bounce a b = case (shape b !!> shape a) of
  Nothing -> return (a, b)
  (Just pushout) -> do
    fireCollision a b pushoutA
    fireCollision b a pushoutB
    return (newA, newB) where
      totalMass = mass a + mass b
      totalElasticity = elasticity a * elasticity b
      relMassA = mass a / totalMass
      relMassB = mass b / totalMass

      pushoutA = mulSV relMassB pushout
      pushoutB = mulSV relMassA (negate pushout)

      zeroMomentumFrame = mulSV relMassA (velocity a) + mulSV relMassB (velocity b)
      relVelA = velocity a - zeroMomentumFrame
      relVelB = velocity b - zeroMomentumFrame
      velChangeA = mulSV (1 + totalElasticity) (negate relVelA)
      velChangeB = mulSV (1 + totalElasticity) (negate relVelB)

      newA = move pushoutA $ applyImpulse velChangeA $ a
      newB = move pushoutB $ applyImpulse velChangeB $ b
