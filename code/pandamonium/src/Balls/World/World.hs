{-# LANGUAGE TemplateHaskell #-}

module Balls.World.World where

import Control.Lens
import Graphics.Gloss ( Picture (Pictures), white, red )
import Common.Renderable
import Common.Redux
import Common.Shapes.Shape
import Common.Entities.Block
import Balls.Entities.Ball

data World = World
  { _walls :: [ Block ]
  , _balls :: [ Ball ]
  }

makeLenses ''World

instance Renderable World where
  render world = Pictures $
                 (render <$> world ^. walls) ++
                 (render <$> world ^. balls)

ballsRedux :: Redux World a
ballsRedux = noOpRedux

world :: World
world = World
 { _walls = [ Block (rectangleV (-400, -400) (800, 20)) white
            , Block (rectangleV (-400, -400) (20, 800)) white
            , Block (rectangleV (380, -400) (20, 800)) white
            , Block (rectangleV (-400, 400) (800, 20)) white
            ]
 , _balls = [ Ball { _mass = 1, _pos = (0, 0), _radius = 20, _vel = (0, 0), _col = red}]
 }
