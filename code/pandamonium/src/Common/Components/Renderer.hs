module Common.Components.Renderer where

import Graphics.Gloss
import Data.Maybe

import Common.Components
import Common.Renderable
import Common.Shapes.Shape

data Renderer = Renderer (Components -> Maybe Picture)

draw :: Components -> Renderer -> Maybe Picture
draw c (Renderer f) = f c

instance Renderable Components where
  render c = fromMaybe Blank (from c >>= draw c)

coloredShape :: Renderer
coloredShape = Renderer (apply2 coloredShape') where
  coloredShape' :: Color -> Shape -> Picture
  coloredShape' c s = color c $ render s
