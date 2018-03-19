module Score where

import Renderable
import Graphics.Gloss

data Score = Score Vector Int

instance Renderable Score where
  render (Score (x, y) points) =
    translate x y
    $ scale 0.25 0.25
    $ Text
    $ show points
