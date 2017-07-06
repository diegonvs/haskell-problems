fatorialIO :: IO()
fatorialIO = do
				putStr "Digite um número: "
				num <- getLine
				putStr "O resultado é: "
				print (fat (read num))

fat :: Int -> Int
fat 0 = 1
fat n = n * fat(n -1)