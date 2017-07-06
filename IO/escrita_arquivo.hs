import System.IO
--import IO --> Depende da versao do haskell
escrever :: IO()
escrever = do
			arq <- openFile "teste.txt" WriteMode
			hPutStr arq "Escrita no arquivo"
			putStrLn "Escrita realizada com sucesso"
			hFlush arq
			hClose arq

escreverFinal :: IO()
escreverFinal = do
			arq <- openFile "teste.txt" AppendMode
			hPutStr arq "\nEscrita no Final"
			putStrLn "Escrita realizada com sucesso"
			hFlush arq
			hClose arq
