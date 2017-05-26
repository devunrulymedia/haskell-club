rows :: [[Int]] -> [[Int]]
rows = id

columns :: [[Int]] -> [[Int]]
columns ([]:rows) = [] 
columns xs = (map head xs) : (columns (map tail xs))
