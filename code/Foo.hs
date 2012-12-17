

-- import Data.List

foo :: [Int] -> ()
foo [] = ()

class ToString a where 
  stringyfy :: a -> String

instance ToString Int where
  stringyfy num = "foo"

instance ToString a => ToString [a] where
 stringyfy [] = ""
 stringyfy (x:xs) = stringyfy x ++ stringyfy xs
  
