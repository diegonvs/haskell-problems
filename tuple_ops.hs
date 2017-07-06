-- fst ("marcos", 73)  --extrai primeiro elemento de uma tupla

-- snd ("marcos", 73)  --extrai segundo elemento de uma tupla de dois elementos

nomes :: (String, String, String)
nomes = ("Marcos", "Geeksbr", "Haskell")


selec_prim(x, _, _) = x
selec_sec(_, y, _) = y
selec_ter(_, _, z) = z
