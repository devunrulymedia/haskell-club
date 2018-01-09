module ShapeTest where

import Test.HUnit
import Shape
import Vector

rect_rect_collisions = TestList [
  TestLabel "Any overlap means there's a collision"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
                  rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 5 }
               in (rectA !!! rectB) @?= True)

  , TestLabel "A gap above first rectangle means no collision"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
                  rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
               in (rectA !!! rectB) @?= False)

   , TestLabel "A gap below first rectangle means no collision"
   $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 50, bottom = 35 }
                   rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
                in (rectA !!! rectB) @?= False)

   , TestLabel "A gap left of the first rectangle means no collision"
   $ TestCase (let rectA = Rectangle { left = 30, right = 50, top = 30, bottom = 15 }
                   rectB = Rectangle { left = 10, right = 25, top = 30, bottom = 15 }
                in (rectA !!! rectB) @?= False)

   , TestLabel "A gap right of the first rectangle means no collision"
   $ TestCase (let rectA = Rectangle { left = 10, right = 25, top = 30, bottom = 15 }
                   rectB = Rectangle { left = 30, right = 50, top = 30, bottom = 15 }
                in (rectA !!! rectB) @?= False)

  ]

rect_rect_pushout = TestList [
  TestLabel "No overlap means no pushout required"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
                  rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 15 }
               in (rectA !!> rectB) @?= Nothing)

  , TestLabel "Overlapping the top of the first rectangle pushes the other rectangle up"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 10, bottom = -10 }
                  rectB = Rectangle { left = 10, right = 30, top = 30, bottom = 5 }
               in (rectA !!> rectB) @?= Just (move_up 5))

  , TestLabel "Overlapping the bottom of the first rectangle pushes the other rectangle down"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
                  rectB = Rectangle { left = 10, right = 30, top = 25, bottom = 5 }
               in (rectA !!> rectB) @?= Just (move_down 5))

  , TestLabel "Overlapping the left of the first rectangle pushes the other rectangle left"
  $ TestCase (let rectA = Rectangle { left = 30, right = 50, top = 40, bottom = 20 }
                  rectB = Rectangle { left = 10, right = 32, top = 40, bottom = 20 }
               in (rectA !!> rectB) @?= Just (move_left 2))

  , TestLabel "Overlapping the right of the first rectangle pushes the other rectangle right"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
                  rectB = Rectangle { left = 27, right = 50, top = 40, bottom = 20 }
               in (rectA !!> rectB) @?= Just (move_right 3))

  , TestLabel "Overlapping a corner pushes out whatever the shortest pushout is #1"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
                  rectB = Rectangle { left = 27, right = 50, top = 22, bottom = 0 }
               in (rectA !!> rectB) @?= Just (move_down 2))

  , TestLabel "Overlapping a corner pushes out whatever the shortest pushout is #2"
  $ TestCase (let rectA = Rectangle { left = 10, right = 30, top = 40, bottom = 20 }
                  rectB = Rectangle { left = 27, right = 50, top = 24, bottom = 0 }
               in (rectA !!> rectB) @?= Just (move_right 3))

  ]

circle_circle_collisions = TestList [
  TestLabel "Circles sharing a centre will always overlap"
  $ TestCase (let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 20 }
                  circB = Circle { centre = Vector { x = 20, y = 30}, radius = 10 }
               in (circA !!! circB) @?= True)

  , TestLabel "Horizontally separated circles touching exactly do not overlap"
  $ TestCase (let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
                  circB = Circle { centre = Vector { x = 120, y = 30}, radius = 40 }
               in (circA !!! circB) @?= False)

  , TestLabel "Vertically separated circles touching exactly do not overlap"
  $ TestCase (let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
                  circB = Circle { centre = Vector { x = 20, y = 130}, radius = 40 }
               in (circA !!! circB) @?= False)

  , TestLabel "Horizontally separated, with sum of radii just exceeding separation"
  $ TestCase (let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
                  circB = Circle { centre = Vector { x = 120, y = 30}, radius = 41 }
               in (circA !!! circB) @?= True)

  , TestLabel "Vertically separated, with sum of radii just exceeding separation"
  $ TestCase (let circA = Circle { centre = Vector { x = 20, y = 30}, radius = 60 }
                  circB = Circle { centre = Vector { x = 20, y = 130}, radius = 41 }
               in (circA !!! circB) @?= True)

  , TestLabel "Diagonally separated, less than sum of radii on any one axis"
  $ TestCase (let circA = Circle { centre = Vector { x = 0, y = 0}, radius = 20 }
                  circB = Circle { centre = Vector { x = 30, y = 40}, radius = 30 }
               in (circA !!! circB) @?= False)

  , TestLabel "Diagonally separated, less than sum of radii on any one axis"
  $ TestCase (let circA = Circle { centre = Vector { x = 0, y = 0}, radius = 21 }
                  circB = Circle { centre = Vector { x = 30, y = 40}, radius = 30 }
               in (circA !!! circB) @?= True)

  ]

circle_circle_pushout = TestList [
    TestLabel "Horizontally non-overlapping circles don't push out"
    $ TestCase ( let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
                     circB = Circle { centre = Vector {x = 20, y = 0}, radius = 5}
                  in ( circA !!> circB) @?= Nothing)

    , TestLabel "Horizontally overlapping circles"
    $ TestCase ( let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
                     circB = Circle { centre = Vector {x = 20, y = 0}, radius = 15}
                  in ( circA !!> circB) @?= Just Vector { x = 5, y = 0 })

    , TestLabel "Vertically overlapping circles"
    $ TestCase ( let circA = Circle {centre = Vector {x = 0, y = 0}, radius = 10}
                     circB = Circle { centre = Vector {x = 0, y = 20}, radius = 15}
                  in ( circA !!> circB) @?= Just Vector { x = 0, y = 5 })
    ]

tests = TestList [ TestLabel "Rectangle-Rectangle Collisions" rect_rect_collisions
                 , TestLabel "Rectangle-Rectangle Pushout" rect_rect_pushout
                 , TestLabel "Circle-Circle Collisions" circle_circle_collisions
                 , TestLabel "Circle-Circle Pushout" circle_circle_pushout ]
