type Nota = Float
type Falta = Integer
type Status = String
type Nome = String
data Registro = Aluno Nome Nota Nota Nota Nota Falta 
				deriving(Show)
carga_horaria :: Float

-- Por padrao foi considerada uma carga horaria de 40 horas
carga_horaria = 40.0

-- Status possíveis:
rf :: Status
rf = "Reprovado por faltas"

rn :: Status
rn = "Reprovado por notas"

am :: Status
am = "Aprovado por media"

ef :: Status
ef = "Exame final"

-- Retorna media das tres notas
md :: Nota -> Nota -> Nota -> Nota
md x y z = ((x+y+z)/3.0)

menorAluno :: Registro -> Registro -> Bool
menorAluno (Aluno x _ _ _ _ _) (Aluno y _ _ _ _ _) | x < y = True
												   | otherwise = False
-- Ordena uma lista de alunos pelo nome
-- Exemplo: [(Aluno "Milton" 10.0 10.0 10.0 0.0 33), (Aluno "Astrobaldo" 3.0 5.0 1.0 5.0 33)]
--         --> [Aluno "Astrobaldo" 3.0 5.0 1.0 5.0 33,Aluno "Milton" 10.0 10.0 10.0 0.0 33]
menores :: Registro -> [Registro] -> [Registro]
menores n [] = []
menores n x = [b | b <- x, menorAluno b n]

maiores :: Registro -> [Registro] -> [Registro]
maiores n [] = []
maiores n x = [b | b <- x, menorAluno n b]

ordenarAluno :: [Registro] -> [Registro]
ordenarAluno [] = []
ordenarAluno (x:[]) = [x]
ordenarAluno (x:xs) = (ordenarAluno (menores x xs)) ++ [x] ++ (ordenarAluno (maiores x xs))

-- Mostra qual foi a nota final e o resultado para um determinado aluno
-- Exemplo: resultado (Aluno "Milton" 10.0 10.0 10.0 0.0 33) --> ("Aprovado por media",10.0)
-- 			resultado (Aluno "Astrobaldo" 3.0 5.0 1.0 5.0 33) --> ("Reprovado por notas",4.0)
--          resultado (Aluno "Irresponsável" 3.0 2.0 1.0 0.0 5) --> ("Reprovado por faltas",0.0)
resultado :: Registro -> (Status, Nota)
resultado (Aluno _ x y z w f) | (fromIntegral(f)/carga_horaria) < 0.75 = (rf, 0.0)
						    | ((md x y z) >= 7.0) = (am, (md x y z))
						    |	(((md x y z) + w)/2.0 < 6.0) = (rn, (((md x y z) + w)/2.0))
						    | otherwise = (ef, (((md x y z) + w)/2.0))

