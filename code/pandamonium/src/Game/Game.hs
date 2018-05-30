{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens
import World.World
import Game.Timer
import Game.GameEvent
import Redux

data Game = Game
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Game

gameRedux :: Redux Game GameEvent
gameRedux = compose
  [ connect timerRedux timer
  , connect worldRedux world
  ]
