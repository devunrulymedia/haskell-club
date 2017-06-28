pack :: (Eq a) => [a] -> [[a]]
pack [] = []
pack xs = prefix : pack (drop (length prefix) xs) where 
	prefix = packfirst xs where
	packfirst [] = []
	packfirst [x] = [x]
	packfirst (x:y:xs) 
    		| x==y = x : packfirst (y:xs)
    		| otherwise = [x]


