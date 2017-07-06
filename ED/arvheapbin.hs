data ArvHB t = Nil | No t (ArvHB t) (ArvHB t)
					deriving (Eq, Ord, Show)


arvHBtoLista :: (Ord t) => ArvHB t ->  t
arvHBtoLista Nil []
arvHBtoLista (No n xt yt)
	= n : merge (arvHBtoLista xt) (arvHBtoLista yt)

arvhb1 = No 4(No 14 Nil Nil) (No 3(No 9 Nil Nil) (No 2 (No 19 Nil Nil) (No 5 Nil Nil)))

merge :: (Ord t) => [t] -> [t]-> [t]
merge [] ys = ys
merge (x:xs []) = x:xs
merge ( x:xs) (y:ys) = if x <= y then x : merge xs (y:ys)
						else y : merge (x:xs) ys


particao :: (t->Bool) -> [t] -> ([t],[t])
particao p xs (filter p xs, filter (not.p) xs)


fazHB :: (Ord t) = [t] -> ArvHB t
fazHB [] = Nil
--fazHB (x:xs) = No x ( fazHB ys) (fazHB zs)

unwrap [t] -> t
[x] = x

fazHeap
...
heapfica :: (Ord t) -> ArvHB t -> ArvHB t
heapfica Nil = Nil
heapfica (No n ae ad) = desliza n (heapfica ae) (heapfica ad)

desliza :: (Ord t) => t -> ArvHB t -> ArvHB t -> ArvHB t
desliza x Nil Nil = No x Nil Nil
desliza x (No y aee aed) Nil = if x <=y then No x (No y aee aed) Nil 
							else No y (desliza x aee aed) Nil
desliza x Nil (No z ade add) = if x <= z then No x Nil (No z ade add)
							else No z Nil (desliza x ade add)
							...