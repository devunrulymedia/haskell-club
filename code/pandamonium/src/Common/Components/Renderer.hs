{-# LANGUAGE DeriveAnyClass #-}

module Common.Components.Renderer where

import Graphics.Gloss
import Data.Maybe

import Common.Components.Entity
import Common.Components.Position
import Common.Renderable
import Common.Shapes.Shape

instance Component Color

instance Component Shape

data Renderer = Renderer (Entity -> Maybe Picture)

maybeDraw :: Renderer -> Entity -> Maybe Picture
maybeDraw (Renderer f) c = f c

draw :: Renderer -> Entity -> Picture
draw (Renderer f) c = fromMaybe Blank (f c)

coloredShape :: Renderer
coloredShape = Renderer (apply3 coloredShape') where
  coloredShape' :: Color -> Position -> Shape -> Picture
  coloredShape' c (Position (x, y)) s = color c $ translate x y $ render s

data Sprite = Sprite Picture deriving Component

data Zoom = Zoom Float deriving Component

sprite :: Renderer
sprite = Renderer sprite' where
  sprite' e = do (Position (x, y)) <- extract e
                 (Sprite spr) <- extract e
                 let (Zoom s) = extractOr (Zoom 1) e
                 return $ translate x y $ scale s s $ spr

composeRenderers :: [ Renderer ] -> Renderer
composeRenderers renderers = Renderer (\e -> Just $ Pictures $ catMaybes $ (flip maybeDraw) e <$> renderers)

spritesAndShapes :: Renderer
spritesAndShapes = composeRenderers [ coloredShape, sprite ]
