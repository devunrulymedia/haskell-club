module Pandamonium.World.Assets where

import Graphics.Gloss
import Common.Graphics.SpriteSheet

data Assets = Assets { pandas :: [Picture], numberSprites :: [Picture] }

loadAssets :: IO Assets
loadAssets = do pandas <- loadSpriteSheet 24 18 "resources/sprites/pandawalk.bmp"
                numbers <- loadSpriteSheet 16 16 "resources/sprites/numbers.bmp"
                return $ Assets pandas numbers
