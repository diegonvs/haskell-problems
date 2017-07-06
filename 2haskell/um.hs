{--
A implementação deste item foi realizada utilizando uma função f2 auxiliar, 
que retorna a lista de elementos a ser concatenada baseada no vetor de posições, 
e a função utilitária map. A função map juntamente com a função implementada dec 
foi usada por conveniencia, para realizar a indexação dos elementos a partir de 
um, embora por default o operador !! use indexação baseada em zero. Adicionalmente
utilizamos a funcao reverse para imprimir os elementos extras na ordem especifica-
da.
--}


dec :: Int -> Int
dec x = x-1

f2 :: [Int] -> [t] -> [t]
f2 [] (y:ys) = []
f2 (x:xs) (y:ys) = ((y:ys)!!x):(f2 xs (y:ys))

f :: [Int] -> [t] -> [t]
f [] [] = []
f (x:xs) [] = []
f [] (y:ys) = (y:ys)
f (x:xs) (y:ys) = (y:ys)++(reverse(f2 (map (dec) (x:xs)) (y:ys)))