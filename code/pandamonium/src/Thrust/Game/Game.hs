{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Thrust.Game.Game where

import Control.Lens
import Thrust.World.World
import Thrust.Game.GameEvent
import Common.Renderable
import Common.Redux
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

adjustZoom :: Event -> Game -> Events Game
adjustZoom (EventKey (Char '+') Down _ _) = return . (mag *~ 1.1)
adjustZoom (EventKey (Char '-') Down _ _) = return . (mag //~ 1.1)
adjustZoom _ = return



gameRedux :: Redux Game
gameRedux = compose
  [ connect worldRedux world
  , noOpRedux { listener = adjustZoom }
  ]

instance Renderable Game where
  render game = scale (game ^. mag) (game ^. mag) $ render (game ^. world)
