{-# LANGUAGE TemplateHaskell #-}

module Balls.World.World where

import Control.Lens
import Graphics.Gloss ( Picture (Pictures), white, red, blue )
import Common.Renderable
import Common.Redux
import Common.Relationship
import Common.Physics.Collisions
import Common.Shapes.Shape
import Common.Entities.Block
import Common.Entities.Entity
import Balls.Entities.Ball
import Balls.Entities.EntityTypes

data World = World
  { _walls :: [ Ent Block ]
  , _balls :: [ Ent Ball ]
  }

makeLenses ''World

collisions :: Float -> World -> Events World
collisions = relationship (onPairs ball_wall_bounce) balls walls where
  ball_wall_bounce t ball wall = bounce_against_static 1 ball wall

collide :: Float -> Ent Ball -> Ent Ball -> Events (Ent Ball, Ent Ball)
collide t = bounce

balls_bouncing :: Float -> World -> Events World
balls_bouncing t w = do newBalls <- againstSelf collide t (w ^. balls)
                        return $ balls .~ newBalls $ w

updateWorld :: Float -> World -> Events World
updateWorld t w = return w
              >>= collisions t
              >>= balls_bouncing t

instance Renderable World where
  render world = Pictures $
                 (render <$> world ^. walls) ++
                 (render <$> world ^. balls)

ballsRedux :: Redux World
ballsRedux = compose
  [ connect (onAll (connect ballRedux edata)) balls
  , noOpRedux { updater = updateWorld }
  ]

world :: World
world = World
 { _walls = [ Entity 0 EWall $ Block (rectangleV (-600, 400) (1200, 20)) white
            , Entity 1 EWall $ Block (rectangleV (-600, 100) (20, 300)) white
            , Entity 2 EWall $ Block (rectangleV (580, 100) (20, 300)) white
            , Entity 3 EWall $ Block (polygon [(-600, 100), (-580, 100), (-180, -300), (-200, -300)]) white
            , Entity 4 EWall $ Block (rectangleV (-200, -600) (20, 300)) white
            , Entity 5 EWall $ Block (rectangleV (-200, -600) (400, 20)) white
            , Entity 6 EWall $ Block (rectangleV (180, -600) (20, 300)) white
            , Entity 7 EWall $ Block (polygon [(600, 100), (580, 100), (180, -300), (200, -300)]) white
            ]
 , _balls = [ Entity 8 EBall $ Ball { _ballMass = 1, _pos = (0, 0), _radius = 30, _vel = (0, 0), _col = red }
            , Entity 9 EBall $ Ball { _ballMass = 2, _pos = (10, 75), _radius = 40, _vel = (0, 600), _col = blue }
            ]
 }
