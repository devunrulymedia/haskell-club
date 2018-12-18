{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}

module Panda2.Game where

import Control.Lens
import Graphics.Gloss (Picture)
import Common.Renderable
import Common.Redux
import Common.Components
import Panda2.Stage

-- A Game is the container for a single play of the Pandamonium game,
-- cycling through various levels, managing flow through the game including
-- tracking when we run out of time/lives and sending appropriate high-score events

data GameAssets = GameAssets
  { _pandaSprites :: [ Picture ]
  }

makeLenses ''GameAssets

data Game = Game
  { _currentWorld :: World
  , _gameAssets :: GameAssets
  }

makeLenses ''Game

initialise :: Game -> Events ()
initialise game = createStage1 (game ^. gameAssets . pandaSprites)

instance Renderable Game where
  render game = render (game ^. currentWorld)

gameRedux :: Redux Game
gameRedux = connect lifecycle currentWorld
