module Common.Components.Renderer where

import Graphics.Gloss
import Data.Maybe

import Common.Components.Entity
import Common.Components.Position
import Common.Renderable
import Common.Shapes.Shape

data Renderer = Renderer (Entity -> Maybe Picture)

draw :: Entity -> Renderer -> Maybe Picture
draw c (Renderer f) = f c

instance Renderable Entity where
  render c = fromMaybe Blank (from c >>= draw c)

coloredShape :: Renderer
coloredShape = Renderer (apply3 coloredShape') where
  coloredShape' :: Color -> Position -> Shape -> Picture
  coloredShape' c (Position x y) s = color c $ translate x y $ render s
