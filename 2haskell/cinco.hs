
f2 :: [t] -> Bool -> [[t]] -> [[t]]
f2 [] _ x = x
f2 (x:xs) True [a, b] = f2 xs False [a++[x], b]
f2 (x:xs) False [a, b] = f2 xs True [a, b++[x]]

f :: [t] -> [[t]]
f x = f2 x True [[], []]

{--
As funções valid e validAux são usadas com propósito de filtrar o espaço de configurações 
do tabuleiro de modo que as rainhas não conflitem em linha ou diagonal. A verificação por 
coluna não é necessária visto que é considerado que as rainhas são colocadas por padrão 
em colunas distintas, sendo o retorno de chess8 uma lista com as posições das linhas das 
rainhas em ordem consecutiva de colunas. A função chess8Aux retorna um conjunto de soluções possíveis
quando passada com o parâmetro 8, e a função chess retorna a solução de posição 1 na lista
de listas retornada por chess8Aux.
--}