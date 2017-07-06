main :: IO () -- IO eh uma monada, utilizada para fazer alguma computacao e produzir valor/resultado
main = do
		putStr "Digite o primeiro numero: "
		n1 <- getLine
		putStr "Digite o segundo numero: "
		n2 <- getLine
		putStrLn ("Soma: " ++ show (read n1 + read n2))

get_char :: IO ()
get_char = do
			putStr "Digite um caractere: "
			c <- getChar 
			putStr "O caractere digitado foi "
			putChar c


somar_numeros :: IO Int
somar_numeros = do
					putStr "Digite o primeiro numero: "
					linha1 <- getLine
					"Digite o segundo numero: " 
					linha2 <- getLine
					return ((read linha1 :: Int) + (read linha2 :: Int))