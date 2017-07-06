--hanoiAux N origem auxiliar destino
--move os N-1 discos superiores para o auxiliar utilizando a torre destino como auxiliar
--move o disco inferior, o maior de todos para a origem
--move os N-1 discos restantes da torre intermediaria para a torre de destino
hanoiAux :: Int -> Char -> Char -> Char-> [[Char]]
hanoiAux 1 o a d = ["Move de " ++ [o] ++ " para " ++ [d]]
hanoiAux n o a d | n > 1 = (hanoiAux (n-1) o d a) ++ ["Move de " ++ [o] ++ " para " ++ [d]] ++ (hanoiAux (n-1) a o d)
				 | otherwise = []

hanoi :: Int -> [[Char]]
hanoi n = (hanoiAux n 'A' 'B' 'C')