type Birds = Int  
type Pole = (Birds,Birds)  

landLeft :: Birds -> Pole -> Maybe Pole  
landLeft n (left,right)  
    | abs ((left + n) - right) < 4 = Just (left + n, right)  
    | otherwise                    = Nothing  
  
landRight :: Birds -> Pole -> Maybe Pole  
landRight n (left,right)  
    | abs (left - (right + n)) < 4 = Just (left, right + n)  
    | otherwise                    = Nothing  



x `pipe` f = f x  


addNumbers :: Maybe Int -> Maybe Int -> Maybe Int
addNumbers a b = a >>= (\x -> b >>= \y -> Just (x + y))

addNumbers' :: Maybe Int -> Maybe Int -> Maybe Int
addNumbers' a b = do x <- a 
                     y <- b
                     return (x + y)