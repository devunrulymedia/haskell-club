module Entities.Panda.Animation where

import Graphics.Gloss
import Renderable
import Entities.Panda.Panda
import Entities.Panda.MovementStateMachine
import Control.Lens

hang_speed :: Float
hang_speed = 150

pixels_per_frame :: Float
pixels_per_frame = 15

runningSprite :: Panda -> Int
runningSprite pd = runningSprite' (pd ^. facing) (pd ^. pos) (pd ^. vel) where
  runningSprite' _ _ (0, _) = 1
  runningSprite' DRight (x, _) _ = 3 + ((floor (x / pixels_per_frame)) `mod` 4)
  runningSprite' DLeft (x, _) _ = 3 + ((floor ((-x) / pixels_per_frame)) `mod` 4)

jumpingSprite :: Panda -> Int
jumpingSprite pd = js' (pd ^. vel) where
  js' (x, y)
    | y > hang_speed = 7
    | y < (-hang_speed) = 9
    | otherwise = 8

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
