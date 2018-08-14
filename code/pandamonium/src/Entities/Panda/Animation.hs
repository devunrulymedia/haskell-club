module Entities.Panda.Animation where

import Graphics.Gloss
import Renderable
import Entities.Panda.Panda
import Entities.Panda.MovementStateMachine
import Control.Lens

hang_speed :: Float
hang_speed = 150

pixels_per_frame :: Float
pixels_per_frame = 30

runningSprite :: Panda -> Int
runningSprite pd = runningSprite' (pd ^. pos) (pd ^. vel) where
  runningSprite' _ (0, _) = 1
  runningSprite' (x, _) _ = [0, 1, 2, 1] !! ((floor (x / pixels_per_frame)) `mod` 4)

jumpingSprite :: Panda -> Int
jumpingSprite pd = js' (pd ^. facing) (pd ^. vel) where
  js' d (x, y)
    | forwards d x && y > hang_speed = 7
    | forwards d x && y < (-hang_speed) = 9
    | y > hang_speed = 10
    | y < (-hang_speed) = 11
    | otherwise = 8

forwards :: Direction -> Float -> Bool
forwards DRight x = x > 0
forwards DLeft x = x < 0

spriteFor :: Panda -> Picture
spriteFor pd = (pd ^. sprites) !! (frame $ pd ^. state) where
  frame Grounded = runningSprite pd
  frame Airborne = jumpingSprite pd
  frame (WallHugging _) = 2

faceRightDirection :: Panda -> Picture -> Picture
faceRightDirection pd = case pd ^. facing of
  DLeft -> scale (-1) 1
  DRight -> id

instance Renderable Panda where
  render pd = let (x, y) = pd ^. pos
               in translate x y
                $ scale 2 2
                $ faceRightDirection pd
                $ spriteFor pd
