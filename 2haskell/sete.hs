pred_length :: Int -> [t] -> Bool
pred_length n x = (length x) >= n

list_sub :: [t] -> Int -> [[t]]
list_sub [] _ = [[]]
list_sub (x:xs) n | (length (x:xs)) >= n = filter (pred_length n) ([x:y | y <- (list_sub xs (n-1))] ++ (list_sub xs n) ) 
					| otherwise = [[]]

{--
Para retornar todas as sublistas de uma lista de elementos, tal que essas sublistas tenham um tamanho
maior ou igual que um número n eh necessário percorrer todo o espaço de busca. Recursivamente, podemos definir 
todas essas combinacoes em uma lista haskell do tipo (x:xs) como o conjunto de todas as sublistas de 
tamanho maiores ou iguais que n que nao contem a cabeça x unido ao conjunto das sublistas da concatenacao
da cabeça x a todas as sublistas possiveis de tamanho maior ou igual a n-1 na lista xs. A funcao filter garante a 
propriedade “>=n”, enquanto a condicao (length (x:xs)) >= n foi utilizada para fins de desempenho, podando 
onde as cadeias nao podem alcançar a longitude minima desejada.
--}