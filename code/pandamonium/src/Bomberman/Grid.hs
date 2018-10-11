module Bomberman.Grid where

import Graphics.Gloss (Vector, white)

import Common.Redux
import Common.Components
import Common.Shapes.Shape

wall :: Vector -> Entity
wall bottomLeft = entity
   <-+ rectangleV bottomLeft (128, 128)
   <-+ white
   <-+ Immovable

wallInGridPosition :: Float -> Float -> Entity
wallInGridPosition x y = wall (128*x, 128*y)

grid :: Events ()
grid = do traverse (\x -> spawn $ wallInGridPosition x 6) [-10 .. 10]
          traverse (\x -> spawn $ wallInGridPosition x (-6)) [-10 .. 10]
          traverse (\y -> spawn $ wallInGridPosition (-10) y) [-5 .. 5]
          traverse (\y -> spawn $ wallInGridPosition 10 y) [-5 .. 5]
          return ()
