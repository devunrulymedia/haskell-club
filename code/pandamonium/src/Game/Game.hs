{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens
import World.World
import Game.Timer
import Game.GameEvent
import Renderable
import Updatable
import Redux

data Game = Game
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Game

withWorld :: World -> Game
withWorld world = Game
  { _world = world
  , _timer = Timer 0 [] 
  }

gameRedux :: Redux Game GameEvent
gameRedux = compose
  [ connect timerRedux timer
  , connect worldRedux world
  ]

instance IORenderable Game where
  iorender game = iorender (game ^. world)

instance IOUpdatable Game where
  iolisten = reduxListen gameRedux
  ioupdate = reduxUpdate gameRedux
