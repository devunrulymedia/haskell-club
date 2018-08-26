module Pandamonium.Systems.Bouncy where

import Graphics.Gloss.Data.Vector
import Common.Redux
import Common.Shapes.Shape
import Pandamonium.Game.GameEvent

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
