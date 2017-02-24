test x y  = do
	putStrLn "Hello World"
	putStrLn "Goodbye World"

and'  a b = a && b
or'   a b = a || b
nand' a b = not (and' a b)
nor'  a b = not (or' a b)
xor'  a b = a /= b
impl' a b = or' (not a) b
equ'  a b = a == b

predicates = [(True, True), (True, False), (False, True), (False, False)]

table_row f a b = (show a) ++ " " ++ (show b) ++ " " ++ (show (f a b))

print_table_row f a b = putStrLn (table_row f a b)

print_table f = do 
	  	 sequence (map (\(a, b) -> print_table_row f a b) predicates)
		 return ()
		
