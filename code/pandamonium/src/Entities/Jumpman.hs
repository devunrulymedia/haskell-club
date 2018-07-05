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
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game
import Graphics.Gloss.Data.Vector
import Game.GameEvent

hv :: Float
hv = 1500

reverseBoost :: Float
reverseBoost = 2.5

jump_power :: Float
jump_power = 500

walljump_power :: Float
walljump_power = 350

jboost :: Float
jboost = 1800

jfuel :: Float
jfuel = 0.2

data GroundedState = Grounded | Ascending | Falling deriving (Eq)

data Jumpman = Jumpman
  { _pos :: Vector
  , _vel :: Vector
  , _fuel :: Float
  , _touching :: Maybe Vector
  , _controller :: Controller
  }

mkJumpman :: Vector -> Controller -> Jumpman
mkJumpman p c = Jumpman p (0, 0) 0 Nothing c

makeLenses ''Jumpman

instance Shaped Jumpman where
  shape jm = let (x, y) = jm ^. pos in rectangle (x-8) (x+8) (y-8) (y+8)

instance Renderable Jumpman where
  render jm = Pictures
    [ color yellow $ render $ shape jm
    , color red $ line $ (jm ^. pos + ) <$> [(0, 0), maybe (0, 0) (mulSV 100) (jm ^. touching)]
    ]

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

collectEvents :: Event -> Jumpman -> Events GameEvent Jumpman
collectEvents event jm = controller %%~ updateController event $ jm

capSpeed :: Jumpman -> Jumpman
capSpeed jm = vel %~ hlimit 600 $ jm

jump :: GameEvent -> Jumpman -> Jumpman
jump (JumpPressed) jm = case (jm ^. touching, jm ^. vel) of
  (Nothing, _) -> jm
  (Just n, (vx, vy)) -> jump' n
    where
      jump' (nx, ny)
        | ny > 0.5   = vel .~ (vx, jump_power) $ fuel .~ jfuel $ jm
        | otherwise = vel .~ (1500 * nx, walljump_power) $ fuel .~ jfuel $ jm

jump _ jm = jm

ascend :: Float -> Jumpman -> Jumpman
ascend t jm = ascend' (jm ^. controller) where
  ascend' c = if jumpPressed c && jm ^. fuel > 0
    then fuel %~ (\x -> x - t)
       $ applyImpulse (0, t * jboost)
       $ jm
    else fuel .~ 0 $ jm

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
processCollisions ResetCollisions jm = touching .~ Nothing $ jm
processCollisions (JumpmanCollision nv) jm =
  let (nx, ny) = normalizeV nv
      v = case jm ^. touching of
        (Nothing)       -> (nx, ny)
        (Just (ox, oy)) -> if ny > oy
          then (nx, ny)
          else (ox, oy)
   in touching .~ Just v $ jm


processCollisions _ jm = jm

updateJumpman :: Float -> Jumpman -> Events GameEvent Jumpman
updateJumpman t jm = return jm
                 <&> moveHorizontally t
                 <&> ascend t
                 <&> capSpeed
                 <&> gravitate t
                 <&> integrate t

reduceJumpman :: GameEvent -> Jumpman -> IOEvents GameEvent Jumpman
reduceJumpman e jm = return jm
                 <&> processCollisions e
                 <&> jump e

listenJumpman :: Event -> Jumpman -> Events GameEvent Jumpman
listenJumpman e jm = return jm
                 >>= collectEvents e

jumpmanRedux :: Redux Jumpman GameEvent
jumpmanRedux = Redux
  { reducer  = reduceJumpman
  , updater  = updateJumpman
  , listener = listenJumpman
  }
