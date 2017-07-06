sub_lists :: [t] -> [[t]]
sub_lists [] = [[]]
sub_lists (x:xs) = [x:y | y <- (sub_lists xs)] ++ (sub_lists xs)

{--
O conjunto de todas as sublistas e subsequencias possiveis da lista (x:xs)
eh a uniao entre o conjunto de todas as sublistas de (xs) e o conjunto de 
todas as concatenacoes [x]++y, tal que y pertence ao conjunto de todas as
sublistas e subsequencias de xs.
--}