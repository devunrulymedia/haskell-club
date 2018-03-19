module Player where

import Paddle
import Score
import Block
import Renderable
import Updatable
import Graphics.Gloss

data Player = Player
                { score :: Score
                , paddle :: Paddle
                , endzone :: Block
                , hue :: Color
                , index :: Int
                }

instance Renderable Player where
  render player = Pictures $
                    color (hue player) <$>
                    [ render $ score player
                    , render $ paddle player
                    , render $ endzone player
                    ]

instance Updatable Player where
  listen event player = player { paddle = listen event $ paddle player }
  update time player = player { paddle = update time $ paddle player }
