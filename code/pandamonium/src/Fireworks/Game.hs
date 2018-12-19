{-# LANGUAGE TemplateHaskell #-}

module Fireworks.Game where

import Control.Lens
import Graphics.Gloss (yellow, green)

import Common.Timer
import Common.Redux
import Common.Renderable
import Common.Components.Renderer
import Common.Components.World

import Fireworks.World
import Fireworks.Entities.Rocket

data Game = Game
  { _world :: World
  , _timer :: Timer
  }

makeLenses ''Game

gameRedux :: Redux Game
gameRedux = compose
  [ connect fireworksRedux world
  , connect timerRedux timer
  ]

instance Renderable Game where
  render game = render (game ^. world)

initialiseGame :: Events ()
initialiseGame = do
  fireEvent (LaunchRocket (0, -500) yellow)
  awaitAction 1 (fireEvent $ LaunchRocket (-300, -300) green)

buildGame :: IO Game
buildGame = do
  let game = Game (newWorld coloredShape) newTimer
  reduxDo gameRedux game initialiseGame
