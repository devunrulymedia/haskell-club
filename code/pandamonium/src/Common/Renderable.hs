module Common.Renderable where

import Graphics.Gloss

class Renderable a where
  render :: a -> Picture

iorender :: Renderable a => a -> IO Picture
iorender x = pure $ render x
