module Updatable where

import Graphics.Gloss.Interface.IO.Game

class IOUpdatable a where
  iolisten :: Event -> a -> IO a
  iolisten event a = pure a
  ioupdate :: Float -> a -> IO a
  ioupdate time a = pure a
