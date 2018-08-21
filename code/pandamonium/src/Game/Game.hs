{-# LANGUAGE TemplateHaskell #-}

module Game.Game where

import Control.Lens
import World.World
import World.Stage
import World.Assets
import World.CreateWorld
import Game.Timer
import Game.GameEvent
import Renderable
import Redux
import Graphics.Gloss (scale)
import Graphics.Gloss.Interface.IO.Game

data Game = Game
  { _world :: World
  , _timer :: Timer
  , _mag :: Float
  , _stages :: [ Stage ]
  , _assets :: Assets
  }

makeLenses ''Game

withStages :: Assets -> [ Stage ] -> Game
withStages stuff (first : rest) = Game
  { _world = createWorld stuff first
  , _timer = Timer 0 [ Pending 1 ChangeScenery ]
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
                   $ timer .~ Timer 0 [ Pending 1 ChangeScenery ]
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

instance IORenderable Game where
  iorender game = scale (game ^. mag) (game ^. mag) <$> iorender (game ^. world)
