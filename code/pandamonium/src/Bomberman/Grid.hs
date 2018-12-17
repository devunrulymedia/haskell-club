module Bomberman.Grid where

import Graphics.Gloss (Vector, white)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

wall :: Vector -> EntityId -> Entity
wall bottomLeft = entity
   <-: rectangleV bottomLeft (128, 128)
   <-: white
   <-: Immovable

wallInGridPosition :: Float -> Float -> EntityId -> Entity
wallInGridPosition x y = wall (128*x, 128*y)

createGrid :: Events ()
createGrid = do traverse (\x -> spawn $ wallInGridPosition x 6) [-10 .. 10]
                traverse (\x -> spawn $ wallInGridPosition x (-6)) [-10 .. 10]
                traverse (\y -> spawn $ wallInGridPosition (-10) y) [-5 .. 5]
                traverse (\y -> spawn $ wallInGridPosition 10 y) [-5 .. 5]
                traverse (\(x, y) -> spawn $ wallInGridPosition x y) [(x, y) | x <- [-8, -6 .. 8], y <- [-4, -2 .. 4]]
                return ()
