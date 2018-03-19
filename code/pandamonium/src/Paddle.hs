module Paddle where

import Controller
import Shape
import Updatable
import Renderable
import Graphics.Gloss
import Graphics.Gloss.Data.Vector
import Graphics.Gloss.Interface.IO.Game

data Paddle = Paddle Point Shape Float Controller

instance Shaped Paddle where
  shape (Paddle pos shp _ _) = move shp pos

instance Movable Paddle where
  move (Paddle pos shp spd cnt) vector = Paddle (pos + vector) shp spd cnt

instance Updatable Paddle where
  listen event (Paddle pos shp spd cnt) = Paddle pos shp spd (updateController event cnt)
  update t (Paddle pos shp spd cnt) = Paddle (moved pos spd cnt) shp spd cnt where
    moved pos spd (Controller (ControlState True False) _) = pos + mulSV t (0, spd)
    moved pos spd (Controller (ControlState False True) _) = pos + mulSV t (0, -spd)
    moved pos spd _ = pos

instance Renderable Paddle where
  render p@(Paddle _ _ _ _) = render $ shape p
