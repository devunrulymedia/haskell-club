module Thrust.Renderable where

import Graphics.Gloss

class Renderable a where
  render :: a -> Picture

class IORenderable a where
  iorender :: a -> IO Picture
