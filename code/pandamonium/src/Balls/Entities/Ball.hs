{-# LANGUAGE TemplateHaskell #-}

module Balls.Entities.Ball where

import Control.Lens
import Graphics.Gloss.Interface.IO.Game (Event (EventKey), Key (Char))
import Graphics.Gloss (Vector, Color, color, yellow)

import Common.Redux
import Common.Renderable
import Common.Shapes.Shape
import Common.Physics.Physics

data Ball = Ball
 { _ballMass :: Float
 , _radius :: Float
 , _pos :: Vector
 , _vel :: Vector
 , _col :: Color
}

makeLenses ''Ball

instance Shaped Ball where
  shape ball = circle (ball ^. pos) (ball ^. radius)

instance Renderable Ball where
  render ball = color (ball ^. col) $ render (shape ball)

instance Movable Ball where
  move vec ball = pos +~ vec $ ball

instance Moving Ball where
  velocity ball = ball ^. vel
  applyImpulse vec ball = vel +~ vec $ ball

instance Physics Ball where
  mass ball = ball ^. ballMass
  elasticity ball = 1

updateBall :: Float -> Ball -> Events Ball
updateBall t b = return b
             <&> gravitate t
             <&> integrate t

ballRedux :: Redux Ball
ballRedux = Redux
  { updater = updateBall
  , listener = noOp
  , reducer = noOp
  }
