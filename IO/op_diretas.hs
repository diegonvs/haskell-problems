escrever :: IO ()
escrever = do
			writeFile "teste.txt" "Aprendendo com sucesso"
			putStrLn "Escrita realizada com sucesso"

ler :: IO ()
ler = do
		conteudo <- readFile "teste.txt"
		putStrLn conteudo

anexar :: IO ()
anexar = do
			appendFile "teste.txt" "\nHaskeel eh legal"
			putStrLn "Conteudo anexado com sucesso"