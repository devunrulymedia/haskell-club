{-# LANGUAGE TemplateHaskell #-}

module Bomberman.Game where

import Control.Lens
import Graphics.Gloss (white, yellow)
import Graphics.Gloss.Interface.IO.Game (Event)

import Common.Redux
import Common.Timer
import Common.Components
import Common.Shapes.Shape
import Common.Renderable

import Bomberman.Bomb
import Bomberman.Bomberman
import Bomberman.Grid

initialise :: Events ()
initialise = do
  spawn bomberman
  createGrid

data BombermanGame = BombermanGame
  { _bworld :: World
  , _btimer :: Timer
  }

makeLenses ''BombermanGame

bgame :: World -> BombermanGame
bgame w = BombermanGame w newTimer

bombermanGame :: IO BombermanGame
bombermanGame = reduxDo bgameRedux (bgame $ newWorld coloredShape) initialise

instance Renderable BombermanGame where
  render bg = render (bg ^. bworld)

bgameRedux :: Redux BombermanGame
bgameRedux = compose [ connect bworldRedux bworld, connect timerRedux btimer ]

bworldRedux :: Redux World
bworldRedux = compose [ connect playerRedux (entities . traverse), bombRedux, physics, lifecycle ]
