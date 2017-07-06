inv_aux :: [t] -> [t] -> [t]
inv_aux [] l_inv = l_inv
inv_aux (x:xs) l_inv = inv_aux xs l_inv++[x] --operador ++ concatena listas necessario transformar x em lista usando []

inv_lista :: [t] -> [t]
inv_lista [] = []
inv_lista l = inv_aux l []


rev_list :: [t] -> [t]
rev_list [] = []
rev_list (x:xs) = rev_list xs ++ [x] -- recursao a esquerda