module Shapes.Collisions.RectangleCollisions where

import Shapes.Collisions.CollisionClass
import Shapes.Movables
import Graphics.Gloss.Data.Vector

data DeRectangle = DeRectangle Float Float Float Float deriving (Show, Eq)

instance Movable DeRectangle where
  move  (x, y) (DeRectangle l r t b) = DeRectangle (l + x) (r + x) (t + y) (b + y)

instance Collides DeRectangle where
  (DeRectangle la ra ta ba) !!! (DeRectangle lb rb tb bb) = not (ta <= bb || tb <= ba || la >= rb || lb >= ra)
  (DeRectangle la ra ta ba) !!> (DeRectangle lb rb tb bb) = foldl1 smallest [ push (0, 1)  (ta - bb),
                                                                              push (0, -1) (tb - ba),
                                                                              push (-1, 0) (rb - la),
                                                                              push (1, 0)  (ra - lb) ] where
                                                               push v m = if m <= 0 then Nothing else Just $ mulSV m v
                                                               shortest a b = if sqMagV a < sqMagV b then a else b
                                                               smallest a b = pure shortest <*> a <*> b
