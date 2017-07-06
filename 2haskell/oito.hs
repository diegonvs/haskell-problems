occur :: String -> String -> String -> Bool
occur [] _  _ = True
occur x _ [] = True
occur _ [] _ = False
occur x (y:ys) (a:as) | y == a  = occur x ys as
				     | otherwise = occur x (ys) x

getT :: Int -> (String, String) -> String
getT 0 (a,b) = a
getT 1 (a,b) = b

-- sep x y z b c
-- retorna uma tupla contendo as string anteriores e posteriores a string x na cadeia y
-- x: A string a ser encontrada
-- y: String onde deveremos buscar a String x
-- z: Restante dos caracteres de x a serem reconhecidos
-- b: Parte da String que coincidiu com o inicio de x, porem ainda esta em reconhecimento
-- c: Parte da String que foi lida mas que nao foi reconhecida
sep :: String -> String -> String -> String -> String -> (String, String)
sep [] y _ _ _= (y, [])
sep x y [] b c = (c, y)
sep _ [] _ b c = ((c++b), [])
sep x (y:ys) (a:as)  b   c  | y == a  = sep x ys as (b++[a]) c
				      		| otherwise = sep x ys x [] (c++b++[y])



substr :: String -> String -> String -> String
substr x y z | (occur x z x) =  (getT 0 (sep x z x [] [])) ++ y ++ (getT 1 (sep x z x [] []))
		     | otherwise = z

{--
Para implementar o reconhecimento de strings, podemos simular um autômato que reconheça a substring1
na substring3 fazendo com que os estados do automato correspondam aos caracteres da substring1,
realizando a transicao dos caracteres um a um. Mantemos a string a ser reconhecida em memoria como 
o primeiro parâmetro da função occur e a cada passo fazemos uma chamada recursiva para a mesma, que 
equivale a uma transicao. O estado atual eh mantido na cabeca da lista correspondente ao terceiro 
parametro, juntamente com o restante a ser reconhecido. Quando não nos restam mais caracteres a 
serem reconhecidos(O terceiro parâmetro é a lista vazia), temos a certeza de que reconhecemos todos os 
caracteres e a substring1 pertence a substring3. Apos o reconhecimento, podemos realizar a 
substituicao com segurança. Em primeiro lugar separamos a string nas duas partes para a esquerda 
e para a direita da substring1, utilizando a função sep. Por ultimo, a funcao substr realiza o resto 
do trabalho concatenando a substring2 no meio das duas substrings resultantes de sep.
--}