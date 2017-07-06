type Pessoa = String
type Carro = String
type Idade = Int
type Registro = (Pessoa, Carro, Idade)
type BD = [Registro]

f_bd :: BD
f_bd  = [("Joao", "Camaro", 20), ("Maria", "Fusca", 30)]

getNome :: Registro -> Pessoa
getNome (n, _, _) = n

getCarros :: BD -> [String]
getCarros ((_, carro, _):xs) = carro : getCarros xs --> retorna lista de todos os carros
