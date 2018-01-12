data RangeComparison = Before | Inside | After

compareRange :: Ord a => a -> a -> a -> RangeComparison
compareRange start end point
  | point < start = Before
  | point <= end  = Inside
  | otherwise     = After

rectCircCollision :: (Vector -> Vector -> Float -> a) -> (Float -> Float -> Float -> a) -> Shape -> Shape -> a
rectCircCollision pointCircleCollision projectionCircleCollision rect@Rectangle {} circ@Circle {} =
   case (compareRange (left a) (right a) (x $ centre b),
         compareRange (bottom a) (top a) (y $ centre b)) of
     (Before, Before) = pointCircleCollision Vector { x = left a,  y = bottom a } (centre b) (radius b)
     (Before, After)  = pointCircleCollision Vector { x = left a,  y = top a    } (centre b) (radius b)
     (After, Before)  = pointCircleCollision Vector { x = right a, y = bottom a } (centre b) (radius b)
     (After, After)   = pointCircleCollision Vector { x = left a,  y = top a    } (centre b) (radius b)
     (Before, Inside) = projectionCircleCollision (left a)   (x $ centre b) (radius b)
     (After, Inside)  = projectionCircleCollision (right a)  (x $ centre b) (radius b)
     (Inside, Before) = projectionCircleCollision (bottom a) (y $ centre b) (radius b)
     (Inside, After)  = projectionCircleCollision (top a)    (y $ centre b) (radius b)

pointCircleCollision :: Vector -> Vector -> Float -> Bool
pointCircleCollision corner centre radius = radius * radius > sq_mag (corner - centre)

projectionCircleCollision :: Float -> Float -> Float -> Bool
pointProjectionCollision lineProj circleProj radius = radius > (abs lineProj circleProj)
