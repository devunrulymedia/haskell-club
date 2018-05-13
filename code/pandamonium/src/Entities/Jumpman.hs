{-# LANGUAGE TemplateHaskell #-}


module Entities.Jumpman where

import Control.Lens
import Shapes.Shape
import Renderable
import Updatable
import Systems.Controller
import Graphics.Gloss (color, yellow, Vector, Picture)

hv :: Float
hv = 500

jv :: Float
jv = 600

data GroundedState = Grounded | Aerial

data Jumpman = Jumpman
  { _pos :: Vector
  , _vel :: Vector
  , _grounded :: GroundedState
  , _controller :: Controller
  }

makeLenses ''Jumpman

instance Shaped Jumpman where
  shape jm = Circle (jm ^. pos) 16

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

instance Updatable Jumpman where
  listen event jm = controller %~ updateController event $ jm
  update t jm = capSpeed . moveHorizontally . jump $ jm where
    capSpeed :: Jumpman -> Jumpman
    capSpeed jm' = vel %~ hlimit 300 $ jm'
    moveHorizontally :: Jumpman -> Jumpman
    moveHorizontally jm' = case jm' ^. controller of
      (Controller (ControlState True False _) _) -> applyImpulse (t*(-hv), 0) $ jm'
      (Controller (ControlState False True _) _) -> applyImpulse (t*hv, 0) $ jm'
      otherwise -> jm'
    jump :: Jumpman -> Jumpman
    jump jm' = case jm' ^. controller of
      (Controller (ControlState _ _ True) _) -> vel %~ (+ (0, jv)) $ controller %~ consumeJump $ jm'
      otherwise -> jm'
