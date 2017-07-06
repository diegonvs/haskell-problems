putStrLn:: String -> IO()
putStrLn str = do putStr str
				  putStr "\n"


faz4vezes::String -> IO()
faz4vezes str = do putStrLn str
				   putStrLn str
				   putStrLn str
				   putStrLn str

fazNvezes :: Int -> String -> IO()
fazNvezes n str = if n <= 1 then putStrLn str
							else do putStrLn str
									fazNvezes (n-1) str


leia2Linhas :: IO()
leia2Linhas = do getLine
				 getLine
				 putStrLn "duas linhas lidas"

getNput :: IO()
getNput = do linha1 <- getLine
			 linha2 <- getLine
			 putStrLn linha1
			 putStrLn linha2
