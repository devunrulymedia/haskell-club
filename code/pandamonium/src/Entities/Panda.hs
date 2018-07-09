{-# LANGUAGE TemplateHaskell #-}


module Entities.Panda (Panda, mkPanda, pandaRedux) where

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

data Panda = Panda
  { _pos :: Vector
  , _vel :: Vector
  , _fuel :: Float
  , _touching :: Maybe Vector
  , _controller :: Controller
  }

mkPanda :: Vector -> Controller -> Panda
mkPanda p c = Panda p (0, 0) 0 Nothing c

makeLenses ''Panda

instance Shaped Panda where
  shape pd = let (x, y) = pd ^. pos in rectangle (x-8) (x+8) (y-8) (y+8)

instance Renderable Panda where
  render pd = Pictures
    [ color yellow $ render $ shape pd
    , color red $ line $ (pd ^. pos + ) <$> [(0, 0), maybe (0, 0) (mulSV 100) (pd ^. touching)]
    ]

instance Movable Panda where
  move dv pd = pos %~ (+dv) $ pd

instance Moving Panda where
  velocity pd = pd ^. vel
  applyImpulse da pd = vel %~ (+da) $ pd

hlimit :: Float -> Vector -> Vector
hlimit mx (x, y)
  | x > mx = (mx, y)
  | x < (-mx) = (-mx, y)
  | otherwise = (x, y)

collectEvents :: Event -> Panda -> Events GameEvent Panda
collectEvents event pd = controller %%~ updateController event $ pd

capSpeed :: Panda -> Panda
capSpeed pd = vel %~ hlimit 600 $ pd

jump :: GameEvent -> Panda -> Panda
jump (JumpPressed) pd = case (pd ^. touching, pd ^. vel) of
  (Nothing, _) -> pd
  (Just n, (vx, vy)) -> jump' n
    where
      jump' (nx, ny)
        | ny > 0.5   = vel .~ (vx, jump_power) $ fuel .~ jfuel $ pd
        | otherwise = vel .~ (1500 * nx, walljump_power) $ fuel .~ jfuel $ pd

jump _ pd = pd

ascend :: Float -> Panda -> Panda
ascend t pd = ascend' (pd ^. controller) where
  ascend' c = if jumpPressed c && pd ^. fuel > 0
    then fuel %~ (\x -> x - t)
       $ applyImpulse (0, t * jboost)
       $ pd
    else fuel .~ 0 $ pd

moveHorizontally :: Float -> Panda -> Panda
moveHorizontally t pd = let (x, y) = velocity pd in case pd ^. controller of
  (Controller (ControlState True False _) _) -> applyImpulse (t*(-hv)*(if x > 0 then reverseBoost else 1), 0) $ pd
  (Controller (ControlState False True _) _) -> applyImpulse (t*hv*(if x < 0 then reverseBoost else 1), 0) $ pd
  otherwise -> vel %~ (slowBy hv) $ pd where
    slowBy :: Float -> Vector -> Vector
    slowBy f (x, y)
      | x > 0 = (x - (min x (t*f)), y)
      | True = (x + (min (-x) (t*f)), y)

processCollisions :: GameEvent -> Panda -> Panda
processCollisions ResetCollisions pd = touching .~ Nothing $ pd
processCollisions (PandaCollision nv) pd =
  let (nx, ny) = normalizeV nv
      v = case pd ^. touching of
        (Nothing)       -> (nx, ny)
        (Just (ox, oy)) -> if ny > oy
          then (nx, ny)
          else (ox, oy)
   in touching .~ Just v $ pd


processCollisions _ pd = pd

updatePanda :: Float -> Panda -> Events GameEvent Panda
updatePanda t pd = return pd
                 <&> moveHorizontally t
                 <&> ascend t
                 <&> capSpeed
                 <&> gravitate t
                 <&> integrate t

reducePanda :: GameEvent -> Panda -> IOEvents GameEvent Panda
reducePanda e pd = return pd
                 <&> processCollisions e
                 <&> jump e

listenPanda :: Event -> Panda -> Events GameEvent Panda
listenPanda e pd = return pd
                 >>= collectEvents e

pandaRedux :: Redux Panda GameEvent
pandaRedux = Redux
  { reducer  = reducePanda
  , updater  = updatePanda
  , listener = listenPanda
  }
