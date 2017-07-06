

myZip :: [Int] -> [Int] -> [(Int,Int)]
myZip [] [] = []
myZip (x:xs) (y:ys) = [(x,y)] ++ myZip xs ys

--myUnzip :: [Int] -> ([Int])
myUnzip [] = ([], []) -- Defaults to a pair of empty lists, not null
myUnzip xs = (map fst xs, map snd xs)

{-myEq :: [t] -> [t] -> Bool
myEq [] [] = True
myEq [] _ = False
myEq _ [] = False
myEq (x:xs) (y:ys) | (x /= y) = False
				   | otherwise = myEq xs ys-}
--problem4
myLen :: [t] -> Int
myLen [] = 0
myLen (x:xs) = 1 + myLen xs


--problem1
myLast :: [t] -> t
myLast (x:xs) | (myLen (x:xs) == 1) = x
			  | otherwise  = myLast xs

--problem2
myButLast :: [t] -> t
myButLast (x:xs) | (myLen(x:xs) == 2) = x
				 | otherwise = myButLast xs


--problem3
elementAt :: [t] -> Int -> t
elementAt (x:xs) n | (n == 1) = x
				   | (n > 1) = elementAt xs (n-1)

--problem5
myReverse :: [t] -> [t]
myReverse [] = []
myReverse (x:xs) = (myReverse xs)++[x]


--problem6
isPalindrome :: [Char] -> Bool
isPalindrome x | (x == (myReverse x)) = True
			   | otherwise = False

--problem7
data NestedList = Elem Int | List [NestedList]

is_elem :: NestedList -> Bool
is_elem (Elem _) = True
is_elem _ = False

get_elem :: NestedList -> Int
get_elem (Elem x) = x

myFlatten :: NestedList -> [Int]
myFlatten (Elem x) = [x]
myFlatten (List []) = []
myFlatten  (List (x:xs)) | is_elem(x) = get_elem(x):myFlatten(List xs)
					    | otherwise = myFlatten(x) ++ myFlatten(List xs)

--problem8
myCompress :: [Char] -> [Char]
myCompress [] = []
myCompress (x:[]) = [x]
myCompress (x:(y:ys)) | x == y = myCompress(y:ys)
					  | otherwise = x:(myCompress(y:ys))

--problem9
packAux :: [Char] -> [Char] -> [[Char]]
packAux [] [] = []
packAux (x:[]) [] = [[x]]
packAux [] (x:[]) = [[x]]
packAux (x:xs) [] = [(x:xs)]
packAux [] (x:xs) = packAux [x] xs
packAux (x:xs) (y:ys) | x == y = packAux (y:(x:xs)) ys
					  | otherwise = (x:xs):(packAux [] (y:ys))

pack :: [Char] -> [[Char]]
pack x = packAux [] x
{-- 
Exemplo:
pack ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = packAux [] ['a', 'a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = packAux ['a'] ['a', 'a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = packAux ['a', 'a'] ['a', 'a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = packAux ["aaa"] ['a', 'b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = packAux ["aaaa"] ['b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']
 = ["aaaa"]:(packAux [] ['b', 'c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
 = ["aaaa"]:(packAux ['b'] ['c', 'c', 'a', 'a', 'd', 'e', 'e', 'e'])
 = ["aaaa"]:(['b']:(packAux [] ['c', 'c', 'a', 'a', 'd', 'e', 'e', 'e']))
 = ["aaaa"]:(['b']:(packAux ['c'] ['c', 'a', 'a', 'd', 'e', 'e', 'e']))
 = ["aaaa"]:(['b']:(packAux ["cc"] ['a', 'a', 'd', 'e', 'e', 'e']))
 = ["aaaa"]:(['b']:(("cc"):(packAux [] ['a', 'a', 'd', 'e', 'e', 'e']))
 = ("aaaa"):(['b']:(("cc"):(packAux ['a'] ['a', 'd', 'e', 'e', 'e']))
 = ["aaaa"]:(['b']:(("cc"):(packAux ("aa") ['d', 'e', 'e', 'e']))
 = ["aaaa"]:(['b']:(("cc"):(("aa"):(packAux [] ['d', 'e', 'e', 'e'])) )
 --}

--problem10
encodeAux :: [[Char]] ->  [(Int,Char)]
encodeAux [] = []
encodeAux ((x:xs):ys) = ((myLen (x:xs)), x):(encodeAux ys)

encode :: [Char] -> [(Int,Char)]
encode x = encodeAux (pack x)

--problem11
data Compression = Single Char | Multiple Int Char
					deriving(Show)

encodeModifiedAux :: [[Char]] -> [Compression]
encodeModifiedAux [] = []
encodeModifiedAux((x:xs):ys) | (myLen (x:xs)) == 1 = (Single x):(encodeModifiedAux ys)
							 | otherwise = (Multiple (myLen(x:xs)) x):(encodeModifiedAux ys)

encodeModified :: [Char] -> [Compression]
encodeModified x = encodeModifiedAux (pack x)

