module Pandamonium.Systems.Physics where

import Graphics.Gloss.Data.Vector
import Pandamonium.Redux
import Pandamonium.Shapes.Shape
import Pandamonium.Game.GameEvent

gravity :: Float
gravity = 1800

gravitate :: Moving a => Float -> a -> a
gravitate t = applyImpulse (0, -(gravity * t))

integrate :: Moving a => Float -> a -> a
integrate t = applyVelocity t

bounce :: (Movable a, Moving a, Shaped a, Shaped b) => Float -> a -> b -> Events GameEvent a
bounce el a b = case (shape b !!> shape a) of
  Nothing -> return a
  (Just pushout) -> do
    fireEvent (PandaCollision offset)
    return (move offset (applyImpulse reflected_vel a)) where
      vel           = velocity a
      unit_push     = normalizeV pushout
      offset        = mulSV (1 + el) pushout
      normal_proj   = (1 + el) * (vel `dotV` unit_push)
      reflected_vel = negate $ mulSV normal_proj unit_push