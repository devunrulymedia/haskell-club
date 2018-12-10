{-# LANGUAGE TemplateHaskell #-}

module Panda2.Assets where

import Control.Lens
import Graphics.Gloss (Picture)
import Common.Graphics.SpriteSheet

data Assets = Assets
  { _pandas :: [ Picture ]
  }

makeLenses ''Assets

loadAssets :: IO Assets
loadAssets = do
  pandas <- loadSpriteSheet 24 18 "resources/sprites/pandawalk.bmp"
  return $ Assets pandas
