{-# LANGUAGE TemplateHaskell #-}


module Thrust.Entities.Thruster where

import Control.Lens
import Control.Arrow
import Common.Shapes.Shape
import Common.Renderable
import Thrust.Systems.Controller
import Graphics.Gloss (color, yellow, green, translate, rotate, Color, Vector, Picture)
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Thrust.Game.GameEvent
import Common.Redux

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
  shape th = let (x, y) = th ^. pos in rectangle (x-8) (x+8) (y-8) (y+8)

instance Renderable Thruster where
  render th = r where
    renderAngle = pi/2 - (argV $ th ^. orientation)
    degrees = (renderAngle / pi) * 180
    r = case th ^. pos of
        (x, y) -> translate x y $ rotate degrees $ th ^. sprite

instance Movable Thruster where
  move dv th = pos %~ (+dv) $ th

instance Moving Thruster where
  velocity th = th ^. vel
  applyImpulse da th = vel %~ (+da) $ th

collectEvents :: Event -> Thruster -> Events Thruster
collectEvents event th = controller %%~ updateController event $ th

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

update :: Float -> Thruster -> Thruster
update t = spin t . thrust t

accelerate :: Float -> Thruster -> Thruster
accelerate t thruster = vel %~ (+ mulSV t (thruster ^. accel)) $ thruster

updateThruster :: Float -> Thruster -> Events Thruster
updateThruster t th = return th
                 <&> update t
                 <&> accelerate t

reduceThruster :: GameEvent -> Thruster -> IOEvents Thruster
reduceThruster e th = return th

listenThruster :: Event -> Thruster -> Events Thruster
listenThruster e th = return th
                 >>= collectEvents e

thrusterRedux :: Redux Thruster
thrusterRedux = Redux
  { reducer  = concrify reduceThruster
  , updater  = updateThruster
  , listener = listenThruster
  }
