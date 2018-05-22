{-# LANGUAGE TemplateHaskell #-}


module Entities.Jumpman where

import Control.Lens
import Control.Arrow
import Shapes.Shape
import Renderable
import Updatable
import Systems.Controller
import Graphics.Gloss (color, yellow, Vector, Picture)

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

jv :: Float
jv = 750

data GroundedState = Grounded | Ascending | Falling

data Jumpman = Jumpman
  { _pos :: Vector
  , _vel :: Vector
  , _grounded :: GroundedState
  , _controller :: Controller
  }

makeLenses ''Jumpman

instance Shaped Jumpman where
  shape jm = let (x, y) = jm ^. pos in rectangle (x-8) (x+8) (y+8) (y-8)

instance Renderable Jumpman where
  render jm = color yellow $ render $ shape jm

instance Movable Jumpman where
  move dv jm = pos %~ (+dv) $ jm

instance Moving Jumpman where
  velocity jm = jm ^. vel
  applyImpulse da jm = vel %~ (+da) $ jm

hlimit :: Float -> Vector -> Vector
hlimit mx (x, y)
  | x > mx = (mx, y)
  | x < (-mx) = (-mx, y)
  | otherwise = (x, y)

collectEvents event jm = controller %~ updateController event $ jm

capSpeed :: Jumpman -> Jumpman
capSpeed jm = vel %~ hlimit 600 $ jm

jump :: Jumpman -> Jumpman
jump jm = case jm ^. controller of
  (Controller (ControlState _ _ True) _) -> vel %~ (+ (0, jv)) $ controller %~ consumeJump $ jm
  otherwise -> jm

moveHorizontally :: Float -> Jumpman -> Jumpman
moveHorizontally t jm = let (x, y) = velocity jm in case jm ^. controller of
  (Controller (ControlState True False _) _) -> applyImpulse (t*(-hv)*(if x > 0 then reverseBoost else 1), 0) $ jm
  (Controller (ControlState False True _) _) -> applyImpulse (t*hv*(if x < 0 then reverseBoost else 1), 0) $ jm
  otherwise -> vel %~ (slowBy hv) $ jm where
    slowBy :: Float -> Vector -> Vector
    slowBy f (x, y)
      | x > 0 = (x - (min x (t*f)), y)
      | True = (x + (min (-x) (t*f)), y)

update :: Float -> Jumpman -> Jumpman
update t = jump
       >>> moveHorizontally t
       >>> capSpeed
