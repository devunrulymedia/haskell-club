numbers :: [Int]
numbers = [1,2,3,4,5]

repeatNTimes :: Int -> [Int]
repeatNTimes n = replicate n n

unfurlBind :: [Int]
unfurlBind = numbers >>= repeatNTimes

unfurlBind2 :: [Int]
unfurlBind2 = numbers
          >>= (\x -> repeatNTimes x)
          >>= (\y -> return y)

unfurlLC :: [Int]
unfurlLC = [ y | x <- numbers, y <- repeatNTimes x ]

unfurl :: [Int]
unfurl = do x <- numbers
            y <- repeatNTimes x
            return y

allPairs :: [(Int, Int)]
allPairs = do first <- numbers
              second <- numbers
              return (first, second)

letsSaySomeThings :: IO ()
letsSaySomeThings = do putStrLn "hello, what is your name?"
                       name <- getLine
                       putStrLn $ "goodbye " ++ name
