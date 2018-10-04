{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Components.Physics where

import Data.Maybe
import Control.Lens
import Graphics.Gloss.Data.Vector

import Common.Redux
import Common.Relationship
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

instance View ExtractedPhysics where
  entityFrom ep = ep ^. ent

instance View ExtractedBarrier where
  entityFrom (ExtractedBarrier _ _ _ ent) = ent

instance Shaped ExtractedPhysics where
  shape ep = let (Position p) = ep ^. pos
              in move p (ep ^. shp)

instance Shaped ExtractedBarrier where
  shape (ExtractedBarrier _ shp _ _) = shp

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

fireCollision :: (View a, View b) => a -> b -> Vector -> Events ()
fireCollision a b v = fireEvent (Collision (entityFrom a) (entityFrom b) v)

extractPhysics :: Entity -> Maybe ExtractedPhysics
extractPhysics e = pure ExtractedPhysics
               <*> extract e
               <*> pure (extractOr (Elasticity 1) e)
               <*> extract e
               <*> extract e
               <*> extract e
               <*> pure e

extractBarrier :: Entity -> Maybe ExtractedBarrier
extractBarrier e = pure ExtractedBarrier
               <*> extract e
               <*> extract e
               <*> pure (extractOr (Elasticity 1) e)
               <*> pure e

collide :: Float -> Entity -> Entity -> Events (Entity, Entity)
collide t a b = do
  (a', b') <- fromMaybe (return (a, b)) (pure bounce <*> extractPhysics a <*> extractPhysics b)
  (b'', a'') <- fromMaybe (return (b', a')) (pure bounce_against_static <*> extractPhysics b' <*> extractBarrier a')
  fromMaybe (return (a'', b'')) (pure bounce_against_static <*> extractPhysics a'' <*> extractBarrier b'')

bounce_against_static :: ExtractedPhysics -> ExtractedBarrier -> Events (Entity, Entity)
bounce_against_static a b = case (shape b !!> shape a) of
 Nothing -> return (entityFrom a, entityFrom b)
 (Just pushout) -> do
   fireCollision a b offset
   fireCollision b a (0, 0)

   return (entityFrom a <-| onPosition (+ offset) <-| onVelocity (+ reflected_vel), entityFrom b) where
     vel           = velocity a
     unit_push     = normalizeV pushout
     elA           = elasticity a
     (ExtractedBarrier _ _ (Elasticity elB) _) = b
     el            = elA * elB
     offset        = mulSV (1 + el) pushout
     normal_proj   = (1 + el) * (vel `dotV` unit_push)
     reflected_vel = negate $ mulSV normal_proj unit_push


bounce :: ExtractedPhysics -> ExtractedPhysics -> Events (Entity, Entity)
bounce a b = case (shape b !!> shape a) of
  Nothing -> return (entityFrom a, entityFrom b)
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

      entA = entityFrom a <-| onPosition (+ pushoutA) <-| onVelocity (+ velChangeA)
      entB = entityFrom b <-| onPosition (+ pushoutB) <-| onVelocity (+ velChangeB)

updatePhysics1 :: Float -> Entity -> Events Entity
updatePhysics1 t e = return e <&> update2 applyVel t <&> update2 applyAcc t

updatePhysics :: Float -> [ Entity ] -> Events [ Entity ]
updatePhysics t es = return es
                 >>= againstSelf collide t

physicsRedux :: Redux [ Entity ]
physicsRedux = Redux
  { updater = composeHandler [ updatePhysics, onEach updatePhysics1 ]
  , listener = noOp
  , reducer = noOp
  }
