{-# LANGUAGE TemplateHaskell #-}

module Pandamonium.Game.Game where

import Control.Lens
import Pandamonium.World.World
import Pandamonium.World.Stage
import Pandamonium.World.Assets
import Pandamonium.World.CreateWorld
import Common.Timer
import Pandamonium.Game.GameEvent
import Common.Renderable
import Common.Redux
import Graphics.Gloss (scale)
import Graphics.Gloss.Interface.IO.Game

data Game = Game
  { _world :: World
  , _timer :: Timer GameEvent
  , _mag :: Float
  , _stages :: [ Stage ]
  , _assets :: Assets
  }

makeLenses ''Game

withStages :: Assets -> [ Stage ] -> Game
withStages stuff (first : rest) = Game
  { _world = createWorld stuff first
  , _timer = Timer 0 []
  , _mag = 2
  , _stages = rest
  , _assets = stuff
  }

adjustZoom :: Event -> Game -> Events GameEvent Game
adjustZoom (EventKey (Char '+') Down _ _) = return . (mag *~ 1.1)
adjustZoom (EventKey (Char '-') Down _ _) = return . (mag //~ 1.1)
adjustZoom _ = return

nextStage :: Game -> Game
nextStage game = let (next : rest) = game ^. stages
                     stuff = game ^. assets
                  in stages .~ rest
                   $ world .~ createWorld stuff next
                   $ timer .~ Timer 0 []
                   $ game

listenForClear :: GameEvent -> Game -> IOEvents GameEvent Game
listenForClear Cleared game = return $ nextStage game
listenForClear _ game = return game

gameRedux :: Redux Game GameEvent
gameRedux = compose
  [ connect timerRedux timer
  , connect worldRedux world
  , noOpRedux { reducer = listenForClear, listener = adjustZoom }
  ]

instance Renderable Game where
  render game = scale (game ^. mag) (game ^. mag) $ render (game ^. world)
