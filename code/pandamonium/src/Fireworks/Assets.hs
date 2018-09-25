{-# LANGUAGE TemplateHaskell #-}

module Fireworks.Assets where

import Control.Lens
import Graphics.Gloss

import Common.Graphics.SpriteSheet

data Assets = Assets { _pandaSprite :: Picture }

makeLenses ''Assets

loadAssets :: IO Assets
loadAssets = do pandas <- loadSpriteSheet 24 18 "resources/sprites/pandawalk.bmp"
                return $ Assets (pandas !! 0)
