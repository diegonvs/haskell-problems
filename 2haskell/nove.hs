type Pessoa = String
type Key = String
type Keys = [Key]
type Nome = String
type Database = [(Pessoa, Livro)]
--Tipos para letra c:
type Dia = Integer
type Mes = Integer
type Ano = Integer
type Data = (Dia, Mes, Ano)
type Livro = (String, Keys, Data) -- Letra b: Acrescentar chaves

numEmprestados :: Database -> Pessoa -> Int
numEmprestados db fulano = length [liv | (pes,liv) <- db, pes == fulano]

livrosEmprestados :: Database -> Pessoa -> [Livro]
livrosEmprestados db fulano = [liv | (pes, liv) <- db, pes == fulano]


--Letra a: Quantidade de livros emprestados deve ser menor ou igual que maxL, o numero maximo que alguem deve tomar emprestado
tomaEmprestado :: Database -> Pessoa -> Livro -> Database
tomaEmprestado db fulano titulo | ((numEmprestados db fulano) < maxL) = (fulano, titulo) : db
								| otherwise = db

--Funcao modificada para facilitar a devolucao do livro apenas pelo nome do livro
devolveLivro :: Database -> Pessoa -> Nome -> Database
devolveLivro ((p, (t,k,d)): r) f l
			|(p == f) && (t == l) = r
			| otherwise = (p,(t,k,d)) : devolveLivro r f l 
devolveLivro [ ] ful tit = error "Nao ha livro emprestado"

--Se livro ocorre no vetor de chaves retorna True. False Caso contrario.
occur :: Key -> Keys -> Bool
occur x (y:ys) | (x == y) = True
			   | otherwise = occur x ys
occur x [] = False

estaEmprestado :: Database -> Nome -> Bool
estaEmprestado db nome = (length [n | (_, (n, k, d)) <- db, n == nome]) > 0

--Letra b: Os livros podem ser encontrados atraves de palavras-chaves associadas a eles
buscarLivro :: Database -> Key -> [Livro]
buscarLivro db k = [(t,l,d) | (_, (t,l,d)) <- db, (occur k l)]


toDias :: Data -> Integer
toDias (d,m,a) = (d + (m*30) + (a*365))


devedores :: Database -> Data -> Nome -> [Pessoa]
devedores db atual l_name = [p | (p, (l,k,d)) <- db, (l == l_name),  ((toDias atual) - (toDias d)) > maxD]


--Letra c: Livros atrasados podem ser encontrados
livrosAtrasados :: Database ->  Data -> [Livro]
livrosAtrasados db atual = [(l,k,d) | (_, (l,k,d)) <- db, ((toDias atual) - (toDias d)) > maxD]

--Letra c: A lista de livros emprestados a uma determinada pessoa pode ser ordenada por data 
ordLivros :: [Livro] -> [Livro]
ordLivros ((pes, keys, dat):xs) = ((ordLivros [(p,k,d) | (p,k,d) <- xs, (toDias d) < (toDias dat)]) ++ [(pes, keys, dat)]) ++ (ordLivros [(p,k,d) | (p,k,d) <- xs, (toDias d) >= (toDias dat)])
ordLivros [] = []

livrosEmprestadosOrd :: Database -> Pessoa -> [Livro]
livrosEmprestadosOrd db p = ordLivros (livrosEmprestados db p)


-- maxL: Numero maximo de livros a serem emprestados
maxL :: Int
maxL = 3

-- maxD: Numero maximo de dias de um emprestimo
maxD :: Integer
maxD = 7

teste = [("Paulo", ("A Mente Nova do Rei", ["IA", "Roger Penrose", "Computador"], (12,6,2014))),
		("Ana", ("O Segredo de Luiza",["Empreendedorismo", "Fernando Dolabela"], (15, 5, 2014))),
		("Paulo", ("O Pequeno Principe", ["Antoine de Saint-Exupery", "Le Petit Prince", "historia"], (28, 6, 2014))), 
		("Mauro", ("O Capital", ["Karl Marx", "Das Kapital", "Socialismo"], (25, 6, 2014))),
		("Francisco", ("O Auto da Compadecida", ["Ariano Suassuna","Peça Teatral"], (22, 6, 2014))),
		("Paulo", ("O Cacador de Pipas", ["historia", "Khaled Hosseini", "Afeganistao"], (12,6,2014)))]

datual :: Data
datual = (30, 6, 2014)