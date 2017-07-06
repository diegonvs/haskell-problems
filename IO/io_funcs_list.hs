hOpenfile :: FilePath -> IOMode -> IOHandle
hClose :: Handle -> IO
hPutChar :: Handle ->String -> IO ()
hPutStrLn :: Handle -> String -> IO()
hPrint :: Show a => Handle -> a -> IO()
hGetChar :: Handle -> IO()
hGetLine :: Handle -> IO()
hGetContents :: Handle -> String

type FilePath = String
writeFile :: FilePath -> String -> IO()
appendFile :: FilePath -> String -> IO()


getChar = hGetChar stdin
putChar = hputChar stdout
