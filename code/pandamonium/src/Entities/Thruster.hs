{-# LANGUAGE TemplateHaskell #-}


module Entities.Thruster where

import Control.Lens
import Control.Arrow
import Shapes.Shape
import Renderable
import Updatable
import Systems.Controller
import Graphics.Gloss (color, yellow, green, translate, rotate, Color, Vector, Picture)
import Graphics.Gloss.Data.Vector
import Game.GameEvent

spinSpeed :: Float
spinSpeed = pi * 2

thrustPower :: Float
thrustPower = 700

gravity :: Vector
gravity = (0,-70)

data Thruster = Thruster
  { _pos :: Vector
  , _vel :: Vector
  , _accel :: Vector
  , _orientation :: Vector
  , _controller :: Controller
  , _sprite :: Picture
  }

makeLenses ''Thruster

instance Shaped Thruster where
  shape th = Circle (th ^. pos) 16

instance Renderable Thruster where
  render th = r where
    renderAngle = pi/2 - (argV $ th ^. orientation)
    degrees = (renderAngle / pi) * 180
    r = case th ^. pos of
        (x, y) -> translate x y $ rotate degrees $ th ^. sprite

instance Movable Thruster where
  move dv th = pos %~ (+dv) $ th

instance Moving Thruster where
<<<<<<< HEAD
  velocity th = th ^. vel
  applyImpulse da th = vel %~ (+da) $ th

collectEvents event th = controller %~ updateController event $ th

spin :: Float -> Thruster -> Thruster
spin t th = case th ^. controller of
  (Controller (ControlState True False _) _) -> orientation %~ rotateV (t * spinSpeed) $ th
  (Controller (ControlState False True _) _) -> orientation %~ rotateV (t * (-spinSpeed)) $ th
  otherwise -> th

thrust :: Float -> Thruster -> Thruster
thrust t th = thruster where
      directionOfThrust = th ^. orientation
      accelDueToThrust = case th ^. controller of
        (Controller (ControlState _ _ True) _) -> (mulSV thrustPower directionOfThrust)
        otherwise -> (0,0)
      totalAccel = gravity + accelDueToThrust
      thruster = accel .~ totalAccel $ th

thrust :: Float -> Thruster -> Thruster
thrust t th = case th ^. controller of
    (Controller (ControlState _ _ True) _) -> accel .~ (5,5) $ th
    otherwise -> accel .~ (0,0) $ th


update :: Float -> Thruster -> Thruster
update t = spin t . thrust t

accelerate :: Float -> Thruster -> Thruster
accelerate t thruster = vel %~ (+ mulSV t (thruster ^. accel)) $ thruster
