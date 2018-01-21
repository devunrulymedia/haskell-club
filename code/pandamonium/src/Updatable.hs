module Updatable where

import Graphics.Gloss.Interface.IO.Game

class Updatable a where
  listen :: Event -> a -> a
  listen event a = a
  update :: Float -> a -> a
  update time a = a
