comp_listas :: [Int] -> [Int] -> Bool
comp_listas [] [] = True
comp_listas (a:b) (c:d)
		| (a==c) = comp_listas b d
		| otherwise = False
comp_listas _ _ = False