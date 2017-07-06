import Data.Matrix

my_sum ::  (Num t) => [t] -> Int -> t
my_sum [] _ = 0
my_sum (x:xs) 1 = x + (my_sum xs (-1))
my_sum (x:xs) (-1) = (-1)*x + (my_sum xs 1)

my_minor :: (Num t) => Matrix t -> Int -> Matrix t
my_minor x a = minorMatrix 1 a x 

my_det :: (Num t) => Matrix t -> t
my_det x | (nrows x) == 2 = ((getElem 1 1 x)*(getElem 2 2 x) - (getElem 1 2 x)*(getElem 2 1 x))
		 | otherwise = (my_sum (map (my_det) (map (my_minor x) [1 .. (ncols x)])) 1)

mat :: Matrix Int
mat = fromList 3 3 [1..]

{--
Na implementação deste exercício foi utilizada a biblioteca Data.Matrix,
que pode ser instalada no linux com o utilitário cabal(apt-get install cabal-install) 
com o comando cabal install matrix.
Instrucoes de instalacao do cabal no windows podem ser encontradas em:
http://www.haskell.org/haskellwiki/Cabal-Install#Windows

Com a declaração de mat é possível realizar o teste da função my_det que recebe como 
argumento uma matriz de números e retorna o determinante dessa matriz, que no caso é zero. 

Foi utilizada a ideia recursiva de que o determinante de uma matriz é igual ao somatório 
do determinante multiplicado por um fator, que se alterna entre -1 e 1(função my_sum) das
submatrizes obtidas escolhendo-se uma linha e removendo, isoladamente, a coluna e o 
elemento de cada elemento desta linha até a coluna ncols x , onde x é uma matriz 
passada como parâmetro.
--}