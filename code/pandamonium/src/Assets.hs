module Assets where

import Graphics.Gloss
import Codec.BMP

data Assets = Assets { ballSprite :: Picture }

loadAssets :: IO Assets
loadAssets = do picture <- loadBMP "resources/sprites/ball.bmp"
                return $ Assets picture
