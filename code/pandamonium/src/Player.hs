module Player where

import Paddle
import Score
import Block
import Renderable
import Updatable
import GameEvent
import Graphics.Gloss

data Player = Player
                { score :: Int
                , scoreLocation :: Point
                , paddle :: Paddle
                , endzone :: Block
                , hue :: Color
                , index :: Int
                }

renderScore :: Player -> Picture
renderScore Player{ score = points, scoreLocation = (x, y) } =
    translate x y
    $ scale 0.25 0.25
    $ Text
    $ show (points)

instance Renderable Player where
  render player = Pictures $
                    color (hue player) <$>
                    [ renderScore player
                    , render $ paddle player
                    , render $ endzone player
                    ]

instance Updatable Player where
  listen event player = player { paddle = listen event $ paddle player }
  update time player = player { paddle = update time $ paddle player }

instance GameEvents Player where
  handleEvent (PointScored i) player = if index player == i
    then player { score = score player + 1 }
    else player
