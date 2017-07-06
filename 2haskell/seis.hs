nove :: [Int]
nove = [4,1,4,2,1,2,1]

cinco :: [Int]
cinco  = [4,2,3,2,4]

um :: [Int]
um = [0,2,1,2,1,2,1,2,1,2,1]

dois :: [Int]
dois = [3,2,5,2,3]

tres :: [Int]
tres = [3,2,4,2,4] 

quatro :: [Int]
quatro = [1,1,2,1,4,2,1,2,1]

seis :: [Int]
seis = [4,2,4,1,4]

sete :: [Int]
sete = [3,2,1,2,1,2,1,2,1]

oito :: [Int]
oito = [4,1,5,1,4]

zero :: [Int]
zero = [4,1,2,1,2,1,4]

-- Letra a
toAst :: Int -> String
toAst 0 = ""
toAst n = "*"++(toAst (n-1))

toSpace :: Int -> String
toSpace 0 = ""
toSpace n = " "++(toSpace (n-1))

toString :: [Int] -> String
toString [] = ""
toString (x:[]) = toAst x
toString (x:y:ys) = (toAst x) ++ (toSpace y) ++ (toString ys)

--Letra b
type Linha = String
toLinhas :: String -> [Linha]
toLinhas [] = []
toLinhas (x:[]) = [[x]]
toLinhas (x:y:z:zs) = [x,y,z]:(toLinhas zs)

--Letra c
showLinhas :: [Linha] -> String
showLinhas [] = ""
showLinhas (x:xs) = x++"\n"++(showLinhas xs)

--Letra d
juntaLinhas :: [Linha] -> [Linha] -> [Linha]
juntaLinhas [] [] = []
juntaLinhas (x:xs) (y:ys) = (x++" "++y):(juntaLinhas xs ys)

--Letra e
ndig :: Int -> Int
ndig n | n < 10 = 1
               | n < 100 = 2
               | otherwise = 3

numeros :: [[Int]]
numeros = [zero,um,dois,tres,quatro,cinco,seis,sete,oito,nove]

tolcd :: Int -> String
tolcd n | (ndig n) == 1 =  showLinhas (toLinhas (toString (numeros!!n)))
        | (ndig n) == 2 
        =  showLinhas
        (juntaLinhas 
              (toLinhas (toString (numeros!!((mod n 100) `div` 10)))) 
              (toLinhas (toString (numeros!!(mod n 10))))
        )
        | (ndig n) == 3 
        = showLinhas
        (juntaLinhas 
          (juntaLinhas
            (toLinhas 
              (toString (numeros!!((mod n 1000) `div` 100)))) 
            (toLinhas 
              (toString (numeros!!((mod n 100) `div` 10)))) ) 
          (toLinhas (toString (numeros!!((mod n 10)))))
        )

--Letra f
type Estado = Bool
tcomp :: String -> Estado -> Int -> [Int]
tcomp [] False n | (n > 0) = [n]
                 | otherwise = []
tcomp [] True n | (n > 0) = [n]
                | otherwise = []
tcomp (x:xs) True n | x == '*' = tcomp (xs) True (n+1)
                    | otherwise = n:(tcomp (x:xs) False 0)
tcomp (x:xs) False n | x == ' ' = tcomp (xs) False (n+1)
                     | otherwise = n:(tcomp (x:xs) True 0)

toCompact :: String -> [Int]
toCompact s = tcomp s True 0
