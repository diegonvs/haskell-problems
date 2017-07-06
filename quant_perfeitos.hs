eDivisor :: Int -> Int -> Bool
eDivisor x y | (x `mod` y == 0) = True
			 | otherwise = False

aux :: Int -> Int -> Int
aux x 1 = 1
aux x y | y <= 0 = 0
		| (eDivisor x y) = (y + (aux x (y-1)))
		| otherwise = (aux x (y-1))

somaDivisores :: Int -> Int
somaDivisores n = (aux n (n `div` 2))

ePerfeito :: Int -> Bool
ePerfeito n = (n == somaDivisores(n))


perfeitosAux :: Int -> Int -> [Int]
perfeitosAux x 0 = []
perfeitosAux x 1 | ePerfeito(x) = [x]
				 | otherwise = perfeitosAux (x+1) 1
perfeitosAux x y | ePerfeito(x) = x:(perfeitosAux (x+1) (y-1))
				 | otherwise = perfeitosAux (x+1) y

quantPerfeitos :: Int -> [Int]
quantPerfeitos 1 = [6]
quantPerfeitos n | n > 1 =  6:(perfeitosAux 7 (n-1))