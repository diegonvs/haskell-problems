push :: [Int] -> Int -> [Int]
push pilha x = x:pilha

pop :: [Int] -> [Int]
pop [] = error "Pilha vazia"
pop (x:xs) = xs

top :: [Int] -> Int
top [] = error "Pilha vazia"
top (x:xs) = x

is_empty :: [Int] -> Bool
is_empty [] = True
is_empty _ = False