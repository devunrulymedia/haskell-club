{-# LANGUAGE TemplateHaskell #-}

module Balls.Entities.Ball where

import Control.Lens
import Graphics.Gloss (Vector, Color, color)

import Common.Renderable
import Common.Shapes.Shape

data Ball = Ball
 { _mass :: Float
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
