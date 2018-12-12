{-# LANGUAGE TemplateHaskell #-}

module Panda2.Session (newSession, sessionRedux) where

import Control.Lens
import Graphics.Gloss (scale)

import Common.Renderable
import Common.Redux
import Common.Components
import Common.Graphics.SpriteSheet
import Panda2.Game
import Panda2.Controller

-- A Session represents a full play session, which can include multiple games,
-- recording high scores, initialising assets and so on. It's the top-level
-- container for the lifespan of the Panda2 program.

data SessionAssets = SessionAssets GameAssets

data Session = Session
  { _game :: Game
  , _sessionAssets :: SessionAssets
  , _controller :: Controller
  }

makeLenses ''Session

instance Renderable Session where
  render session = scale 8 8 $ render (session ^. game)

sessionRedux :: Redux Session
sessionRedux = compose
  [ connect gameRedux game
  , connect (listenRedux listenController) controller
  ]

newSession :: IO Session
newSession = do
  gameAssets <- GameAssets <$> loadSpriteSheet 24 18 "resources/sprites/pandawalk.bmp"
  let game = Game (newWorld spritesAndShapes) gameAssets
  let assets = SessionAssets gameAssets
  let controller = newController 1 ('z', 'x') 'm'
  let session = Session game assets controller
  reduxDo sessionRedux session (initialise game)
