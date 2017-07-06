{-0 0
encher2 x -> y
x 4
0 4
completar1 x -> y
(x + min(y, 3-x)) (y - min(y,3-x))
3 1

esvaziar1 x -> y
0 y
0 1

completar1 x -> y
(x + min(y, 3-x)) (y - min(y, 3-x))
1 0

------------------------------------------------------
0 0
encher1
3 0
completar2
0 3
encher 1
3 3
completar 2
2 4-}
--x possui capacidade 3
--y possui capacidade 4
--inicialmente x e y devem estar vazios ou seja, deve-ser chamar resolver (0,0)

my_min :: (Int, Int) -> Int
my_min (x,y) | (x < y) = x
			 | otherwise = y
encher1 :: (Int, Int) -> (Int, Int)
encher1 (x,y) = (3,y)

completar2 :: (Int, Int) -> (Int, Int)
completar2 (x,y) = ( (x - my_min(x,(4-y))),(y + my_min(x,(4-y))) )

resolver :: (Int, Int) ->[(String, (Int,Int))]
resolver (x,y) = [("Encher 1", (encher1(x,y))), ("Completar 2 com 1", completar2(encher1(x,y))), ("Encher 1",encher1(completar2(encher1(x,y)))), ("Completar 2 com 1",completar2(encher1(completar2(encher1(x,y)))))]
