module Shapes.Polygon where

import Graphics.Gloss.Data.Vector
import Shapes.Datatypes
import Data.Maybe

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
  determineWinding a b = case signum $ (cw_normal a) `dotV` b of
    0    -> Nothing
    1    -> Just Clockwise
    (-1) -> Just Anticlockwise

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

cw_normal :: Vector -> Vector
cw_normal (x, y) = (y, (-x))

normals :: [ Vector ] -> [ Vector ]
normals points = normalizeV <$> ac_normal <$> edges points

polygon :: [ Vector ] -> Shape
polygon points = let clockwise = windClockwise points
                  in Polygon clockwise (normals clockwise)
