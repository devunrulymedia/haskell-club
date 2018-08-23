{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens
import World.World
import Game.Timer
import Game.GameEvent
import Renderable
import Redux
import Graphics.Gloss ( blue, scale )
import Graphics.Gloss.Interface.IO.Game

data Game = Game
  { _world :: World
  , _mag :: Float
  }

makeLenses ''Game

withWorld :: World -> Game
withWorld world = Game
  { _world = world
  , _mag = 2
  }

adjustZoom :: Event -> Game -> Events GameEvent Game
adjustZoom (EventKey (Char '+') Down _ _) = return . (mag *~ 1.1)
adjustZoom (EventKey (Char '-') Down _ _) = return . (mag //~ 1.1)
adjustZoom _ = return



gameRedux :: Redux Game GameEvent
gameRedux = compose
  [ connect worldRedux world
  , noOpRedux { listener = adjustZoom }
  ]

instance IORenderable Game where
  iorender game = scale (game ^. mag) (game ^. mag) <$> iorender (game ^. world)
