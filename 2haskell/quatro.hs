validAux :: Int -> Int -> [Int] -> Bool
validAux _ _ [] = True
validAux p c (x:xs) | p == x = False --rainhas na mesma linha
					| ((x-p) == c) || ((x-p) == (-c)) = False -- rainhas na mesma diagonal
					| otherwise = validAux p (c+1) xs

valid :: [Int] -> Bool
valid [] = True
valid (x:xs) = (validAux x 1 xs) && (valid xs)


--chess8Aux n
--resolve problema das 8 rainhas. O parametro n eh o numero de colunas restantes
chess8Aux :: Int -> [[Int]]
chess8Aux 0 = [[]]
chess8Aux 1 = [[1]]
chess8Aux n = filter valid [x:y | y <- (chess8Aux (n-1)), x <- [1 .. 8]]

chess8 :: [Int]
chess8 = (chess8Aux 8)!!1

{--
As funções valid e validAux são usadas com propósito de filtrar o espaço de configurações do tabuleiro 
de modo que as rainhas não conflitem em linha ou diagonal. A verificação por coluna não é necessária 
visto que é considerado que as rainhas são colocadas por padrão em colunas distintas, sendo o retorno 
de chess8 uma lista com as posições das linhas das rainhas em ordem de colunas. A função chess8Aux 
retorna um conjunto de soluções possíveis quando passada com o parâmetro 8, e a função chess retorna 
a solução de posição 1 na lista de listas retornada por chess8Aux.
--}