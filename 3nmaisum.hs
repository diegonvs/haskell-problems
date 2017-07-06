par :: Int -> Bool
par x | ((mod x 2) == 0) = True
	  | otherwise = False


tamciclo :: Int -> Int
tamciclo 1  = 1
tamciclo x | (par(x) == True) = (1+tamciclo((x `div` 2)))
		   | otherwise = (1+tamciclo(((3*x) + 1)))
