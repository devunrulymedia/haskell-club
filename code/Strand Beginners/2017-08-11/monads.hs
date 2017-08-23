a = [(x,y) | x <- [1,2,3], y <- [10..13]]

b = do x <- [1,2,3]
       y <- [10..13]
       return (x,y)

data Shape = Circle Int Int Int | Box Int Int Int Int

boxArea :: Int -> Int -> Int -> Int -> Maybe Int


area :: Maybe Shape -> Maybe Int
area s = do (Box t b l r) <- s
            boxArea t b l r
