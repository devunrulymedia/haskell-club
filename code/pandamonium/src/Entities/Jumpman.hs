{-# LANGUAGE TemplateHaskell #-}


module Entities.Jumpman (Jumpman, mkJumpman, jumpmanRedux) where

import Control.Lens
import Control.Arrow
import Control.Monad
import Shapes.Shape
import Renderable
import Redux
import Systems.Controller
import Systems.Physics
import Graphics.Gloss (color, yellow, green, blue, Color, Vector, Picture)
import Graphics.Gloss.Interface.IO.Game (Event)
import Game.GameEvent

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

jv :: Float
jv = 750

data GroundedState = Grounded | Ascending | Falling deriving (Eq)

data Jumpman = Jumpman
  { _pos :: Vector
  , _vel :: Vector
  , _grounded :: GroundedState
  , _controller :: Controller
  }

mkJumpman :: Vector -> Controller -> Jumpman
mkJumpman p c = Jumpman p (0, 0) Falling c

makeLenses ''Jumpman

instance Shaped Jumpman where
  shape jm = let (x, y) = jm ^. pos in rectangle (x-8) (x+8) (y+8) (y-8)

instance Renderable Jumpman where
  render jm = color (colorOf jm) $ render $ shape jm

instance Movable Jumpman where
  move dv jm = pos %~ (+dv) $ jm

instance Moving Jumpman where
  velocity jm = jm ^. vel
  applyImpulse da jm = vel %~ (+da) $ jm

colorOf :: Jumpman -> Color
colorOf jm = case jm ^. grounded of
  Grounded -> green
  Falling -> yellow
  Ascending -> blue

hlimit :: Float -> Vector -> Vector
hlimit mx (x, y)
  | x > mx = (mx, y)
  | x < (-mx) = (-mx, y)
  | otherwise = (x, y)

collectEvents :: Event -> Jumpman -> Jumpman
collectEvents event jm = controller %~ updateController event $ jm

capSpeed :: Jumpman -> Jumpman
capSpeed jm = vel %~ hlimit 600 $ jm

jump :: Jumpman -> Jumpman
jump jm = case jm ^. controller of
  (Controller (ControlState _ _ True) _) ->
    controller %~ consumeJump $
    if jm ^. grounded == Grounded
     then grounded .~ Ascending
        $ vel %~ (+ (0, jv))
        $ jm
     else jm
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



processCollisions :: GameEvent -> Jumpman -> Jumpman
processCollisions ResetCollisions jm = case jm ^. grounded of
  Grounded  -> grounded .~ Falling $ jm
  otherwise -> jm
processCollisions (JumpmanCollision (x, y)) jm = if y > 0
  then set grounded Grounded jm
  else jm
processCollisions _ jm = jm

updateJumpman :: Float -> Jumpman -> Events GameEvent Jumpman
updateJumpman t jm = return jm
                 <&> jump
                 <&> moveHorizontally t
                 <&> capSpeed
                 <&> gravitate t
                 <&> integrate t

reduceJumpman :: GameEvent -> Jumpman -> IOEvents GameEvent Jumpman
reduceJumpman e jm = return jm
                 <&> processCollisions e

listenJumpman :: Event -> Jumpman -> Events GameEvent Jumpman
listenJumpman e jm = return jm
                 <&> collectEvents e

jumpmanRedux :: Redux Jumpman GameEvent
jumpmanRedux = Redux
  { reducer  = reduceJumpman
  , updater  = updateJumpman
  , listener = listenJumpman
  }
