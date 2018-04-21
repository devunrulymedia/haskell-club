module Updatable where

import Graphics.Gloss.Interface.IO.Game

class Updatable a where
  listen :: Event -> a -> a
  listen event a = a
  update :: Float -> a -> a
  update time a = a

class IOUpdatable a where
  iolisten :: Event -> a -> IO a
  iolisten event a = pure a
  ioupdate :: Float -> a -> IO a
  ioupdate time a = pure a
