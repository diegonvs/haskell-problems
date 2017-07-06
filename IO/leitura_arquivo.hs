import System.IO

ler_arq :: IO ()
ler_arq = do
			arq <- openFile "teste.txt" ReadMode
			conteudo <- hGetContents arq
			putStrLn conteudo
			hClose arq

