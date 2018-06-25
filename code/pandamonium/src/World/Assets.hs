module World.Assets where

import Graphics.Gloss
import Graphics.SpriteSheet

data Assets = Assets { ballSprite :: Picture, numberSprites :: [Picture] }

loadAssets :: IO Assets
loadAssets = do picture <- loadBMP "resources/sprites/ball.bmp"
                numbers <- loadSpriteSheet 16 16 "resources/sprites/numbers.bmp"
                return $ Assets picture numbers
