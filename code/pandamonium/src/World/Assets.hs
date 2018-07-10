module World.Assets where

import Graphics.Gloss
import Graphics.SpriteSheet

data Assets = Assets { ballSprite :: Picture, shipSprite :: Picture,  numberSprites :: [Picture] }

loadAssets :: IO Assets
loadAssets = do picture <- loadBMP "resources/sprites/ball.bmp"
                numbers <- loadSpriteSheet 16 16 "resources/sprites/numbers.bmp"
                ship <- loadBMP "resources/sprites/craft.bmp"
                return $ Assets picture ship numbers
