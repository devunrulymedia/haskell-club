{-# LANGUAGE Strict #-}

module Common.Shapes.Shapes where

import Graphics.Gloss.Data.Vector
import Common.Shapes.Movables
import Data.Maybe

data Shape = Circle Vector Float
           | Polygon [ Vector ] [ Vector ]
           deriving (Show, Eq)

class Shaped t where
  shape :: t -> Shape

instance Movable Shape where
  move v (Polygon ps ns) = Polygon ((v +) <$> ps) ns
  move v (Circle c r) = Circle (c + v) r

duplicateEnd :: [ Vector ] -> [ Vector ]
duplicateEnd [] = []
duplicateEnd xs = (last xs) : xs

edges :: [ Vector ] -> [ Vector ]
edges points = edges' $ duplicateEnd points where
  edges' (a : b : rest) = (b - a) : edges' (b : rest)
  edges' _ = []

data Winding = Clockwise | Anticlockwise deriving (Eq, Show)

windClockwise :: [ Vector ] -> [ Vector ]
windClockwise points = let windings = catMaybes $ allWindings $ duplicateEnd $ edges points
                        in reverseIfNeeded windings where
  determineWinding :: Vector -> Vector -> Maybe Winding
  determineWinding a b = case signum $ (ac_normal a) `dotV` b of
    0    -> Nothing
    1    -> Just Anticlockwise
    (-1) -> Just Clockwise

  allWindings :: [ Vector ] -> [ Maybe Winding ]
  allWindings (a:b:rest) = determineWinding a b : allWindings (b:rest)
  allWindings _ = []

  reverseIfNeeded :: [ Winding ] -> [ Vector ]
  reverseIfNeeded windings
    | all (== Clockwise) windings = points
    | all (== Anticlockwise) windings = reverse points
    | otherwise = error "Concave polygon"

ac_normal :: Vector -> Vector
ac_normal (x, y) = ((-y), x)

ns :: [ Vector ] -> [ Vector ]
ns points = normalizeV <$> ac_normal <$> edges points

polygon :: [ Vector ] -> Shape
polygon points = let clockwise = windClockwise points
                  in Polygon clockwise (ns clockwise)

rectangle :: Float -> Float -> Float -> Float -> Shape
rectangle l r t b = polygon
  [ (min l r, min t b)
  , (min l r, max t b)
  , (max l r, max t b)
  , (max l r, min t b)
  ]

rectangleV :: Vector -> Vector -> Shape
rectangleV (x,y) (w,h) = polygon
  [ (x, y)
  , (x, y+h)
  , (x+w, y+h)
  , (x+w, y)
  ]

circle :: Vector -> Float -> Shape
circle = Circle
