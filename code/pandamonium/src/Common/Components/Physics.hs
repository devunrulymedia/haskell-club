{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Components.Physics where

import Data.Maybe
import Control.Lens (makeLenses, (^.), (.~))
import Graphics.Gloss.Data.Vector

import Common.Redux
import Common.Physics.Physics (Physics, mass, elasticity)
import Common.Components.Entity
import Common.Components.Position
import Common.Components.Renderer
import Common.Shapes.Shape

data Collision = Collision Entity Entity Vector deriving ReduxEvent

data Mass = Mass Float deriving Component

data Immovable = Immovable deriving Component

data Elasticity = Elasticity Float deriving Component

data ExtractedPhysics = ExtractedPhysics
  { _m :: Mass
  , _el :: Elasticity
  , _shp :: Shape
  , _pos :: Position
  , _vel :: Velocity
  , _ent :: Entity
  }

data ExtractedBarrier = ExtractedBarrier Immovable Shape Elasticity Entity

makeLenses ''ExtractedPhysics

instance Shaped ExtractedPhysics where
  shape ep = let (Position p) = ep ^. pos
              in move p (ep ^. shp)

instance Movable ExtractedPhysics where
  move vec ep = let (Position p) = ep ^. pos
                 in pos .~ (Position (p + vec)) $ ep

instance Moving ExtractedPhysics where
  velocity ep = let (Velocity v) = ep ^. vel in v
  applyImpulse vec ep = let (Velocity v) = ep ^. vel
                         in vel .~ (Velocity (v + vec)) $ ep

instance Physics ExtractedPhysics where
  mass ep = let (Mass ms) = ep ^. m in ms
  elasticity ep = let (Elasticity e) = ep ^. el in e

fireCollision :: ExtractedPhysics -> ExtractedPhysics -> Vector -> Events ()
fireCollision a b v = fireEvent (Collision (a ^. ent) (b ^. ent) v)

extractPhysics :: Entity -> Maybe ExtractedPhysics
extractPhysics e = pure ExtractedPhysics
               <*> from e
               <*> Just (fromMaybe (Elasticity 1) (from e))
               <*> from e
               <*> from e
               <*> from e
               <*> Just e

extractBarrier :: Entity -> Maybe ExtractedBarrier
extractBarrier e = pure ExtractedBarrier
               <*> from e
               <*> from e
               <*> Just (fromMaybe (Elasticity 1) (from e))
               <*> Just e

bounce :: ExtractedPhysics -> ExtractedPhysics -> Events (Entity, Entity)
bounce a b = case (shape b !!> shape a) of
  Nothing -> return (a ^. ent, b ^. ent)
  (Just pushout) -> do
    fireCollision a b pushoutA
    fireCollision b a pushoutB
    return (entA, entB) where
      totalMass = mass a + mass b
      totalElasticity = elasticity a * elasticity b
      relMassA = mass a / totalMass
      relMassB = mass b / totalMass

      pushoutA = mulSV (relMassB * totalElasticity) pushout
      pushoutB = mulSV (relMassA * totalElasticity) (negate pushout)

      zeroMomentumFrame = mulSV relMassA (velocity a) + mulSV relMassB (velocity b)
      relVelA = velocity a - zeroMomentumFrame
      relVelB = velocity b - zeroMomentumFrame

      unit_push = normalizeV pushout
      normal_proj_a = (1 + totalElasticity) * (relVelA `dotV` unit_push)
      normal_proj_b = (1 + totalElasticity) * (relVelB `dotV` unit_push)

      velChangeA = negate $ mulSV normal_proj_a unit_push
      velChangeB = negate $ mulSV normal_proj_b unit_push

      newA = move pushoutA $ applyImpulse velChangeA $ a
      newB = move pushoutB $ applyImpulse velChangeB $ b

      entA = (a ^. ent) <-+ (a ^. pos) <-+ (a ^. vel)
      entB = (b ^. ent) <-+ (b ^. pos) <-+ (b ^. vel)
