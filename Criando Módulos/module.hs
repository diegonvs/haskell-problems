module Funcoes (ehPar, ehImpar) where -- explicita que somente a funcao ehPar esta no modulo

ehPar :: Int -> Bool
ehPar n | (mod n 2 == 0) = True
		| otherwise = False

ehImpar :: Int -> Bool
ehImpar n | (mod n 2 == 0) = False
		  | otherwise = True

