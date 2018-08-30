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

ballsRedux :: Redux World
ballsRedux = compose
  [ connect (onAll ballRedux) balls
  ]

world :: World
world = World
 { _walls = [ Block (rectangleV (-600, 400) (1200, 20)) white
            , Block (rectangleV (-600, 100) (20, 300)) white
            , Block (rectangleV (580, 100) (20, 300)) white
            , Block (polygon [(-600, 100), (-580, 100), (-180, -300), (-200, -300)]) white
            , Block (rectangleV (-200, -600) (20, 300)) white
            , Block (rectangleV (-200, -600) (400, 20)) white
            , Block (rectangleV (180, -600) (20, 300)) white
            , Block (polygon [(600, 100), (580, 100), (180, -300), (200, -300)]) white
            ]
 , _balls = [ Ball { _mass = 1, _pos = (0, 0), _radius = 20, _vel = (0, 0), _col = red}]
 }
