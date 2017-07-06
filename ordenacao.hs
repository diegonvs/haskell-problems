get_menor :: [Int] -> Int
get_menor [x] = x
get_menor (x:xs) | (x < get_menor xs) = x
				 | otherwise = get_menor xs

remove_menor :: [Int] -> [Int]
remove_menor [] = []
remove_menor (x:xs) | (x == get_menor (x:xs)) = xs
					| otherwise (x:remove_menor xs)



aux_ordena :: [Int] -> [Int]
aux_ordena lista_ordenada [] = lista_ordenada
aux_ordena lista_ordenada x = aux_ordena (lista_ordenada++[get_menor x]) (remove_menor x)

ordena :: [Int] -> [Int]
ordena [] = []
ordena x = aux_ordena [] x

