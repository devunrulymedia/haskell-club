{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE TemplateHaskell #-}

module Common.Components.Physics where

import Control.Lens

import Common.Physics.Physics (Physics)
import Common.Components.Entity
import Common.Components.Position
import Common.Shapes.Shape

data Mass = Immovable | Mass Float deriving Component

data Elasticity = Elasticity Float deriving Component

data ExtractedPhysics = ExtractedPhysics
  { _mass :: Mass
  , _elasticity :: Elasticity
  , _eshape :: Shape
  , _position :: Position
  , _velocity :: Velocity
  }

makeLenses ''ExtractedPhysics

instance Shaped ExtractedPhysics where
  shape ep = let (Position p) = ep ^. position
              in move p (ep ^. eshape)
--
-- instance Renderable Ball where
--   render ball = color (ball ^. col) $ render (shape ball)
--
-- instance Movable Ball where
--   move vec ball = pos +~ vec $ ball
--
-- instance Moving Ball where
--   velocity ball = ball ^. vel
--   applyImpulse vec ball = vel +~ vec $ ball
--
-- instance Physics Ball where
--   mass ball = ball ^. ballMass
--   elasticity ball = 1
