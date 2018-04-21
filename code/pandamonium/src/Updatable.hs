module Updatable where

import Graphics.Gloss.Interface.IO.Game

class Updatable a where
  listen :: Event -> a -> a
  listen event a = a
  iolisten :: Event -> a -> IO a
  iolisten event a = pure $ listen event a
  update :: Float -> a -> a
  update time a = a
  ioupdate :: Float -> a -> IO a
  ioupdate time a = pure $ update time a
