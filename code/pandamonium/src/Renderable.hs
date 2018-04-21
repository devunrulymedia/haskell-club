module Renderable where

import Graphics.Gloss

class Renderable a where
  render :: a -> Picture
  iorender :: a -> IO Picture
  iorender a = pure $ render a
