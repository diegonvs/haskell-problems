data ArvBusBin t = Nil | No (ArvBusBin t) t (ArvBusBin t)
						deriving (Show, Ord, Eq)

arvbusbin1 = No (No Nil 9 Nil) 14 (No Nil 19 (No Nil 51 Nil))
arvbusbin2 = No (No Nil 3 Nil) 7 (No Nil 18 (No Nil 27 Nil))

arvBBtolista :: ArvBusBin t -> [t]
arvBBtolista Nil = []
arvBBtolista (No ae n ad) = arvBBtolista ae ++ [n] ++ arvBBtolista ad


membroABB :: (Ord t) => t -> ArvBusBin t -> Bool
membroABB :: x Nil = False
membroABB :: x (No ae y ad)
			| x == y = True
			| x != y = (membroABB x ae) || (membroABB x ad)

altABB :: (Ord t) => ArvBusBin t -> Int
altABB Nil = 0
altABB (No ae n ad)
	|ae == Nil) && (ad == Nil) = 0
	| otherwise 1 + max (altABB ae) (altABB ad)

fazABB :: (Ord t) => [t] -> ArvBusBin t
fazABB :: [] = Nil
fazABB (x:xs) = No (fazABB ys) x (fazABB zs)
						| where (ys,zs) = particao (<=x) xs

particao :: (Ord t) => (t->Bool) -> [t] -> ([t],[t])
partica p xs = (filter p xs, filter (not.p) xs)

sort :: (Ord t) => [t] -> [t]
sort = arvBBtolista.fazABB

remove :: (Ord t) => t -> ArvBusBin t -> ArvBusBin t
remove x Nil = Nil
remove x (No ae n ad)
	| (x<n) = No (remove x ae) n ad
	| (x>n) = No ae n (remove x ad)
	| (x==n) = junta ae ad

junta :: (Ord t) => ArvBusBin t -> ArvBusBin t -> ArvBusBin t
junta Nil yt = yt
junta (No ut x vt) yt = No ut x (junta yt vt)