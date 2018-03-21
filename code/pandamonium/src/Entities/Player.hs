{-# LANGUAGE TemplateHaskell #-}

module Entities.Player where

import Control.Lens

import Entities.Paddle
import Entities.Block
import Renderable
import Updatable
import World.GameEvent
import Graphics.Gloss

data Player = Player
                { _score :: Int
                , _scoreLocation :: Point
                , _paddle :: Paddle
                , _endzone :: Block
                , _hue :: Color
                , _playerNumber :: Int
                }

makeLenses ''Player

renderScore :: Player -> Picture
renderScore Player{ _score = points, _scoreLocation = (x, y) } =
    translate x y
    $ scale 0.25 0.25
    $ Text
    $ show (points)

instance Renderable Player where
  render player = Pictures $
                    color (player ^. hue) <$>
                    [ renderScore player
                    , render $ player ^. paddle
                    , render $ player ^. endzone
                    ]

instance Updatable Player where
  listen event = paddle %~ listen event
  update time = paddle %~ update time

instance GameEvents Player where
  handleEvent (PointScored i) player = if player ^. playerNumber == i
    then score %~ (+1) $ player
    else player
