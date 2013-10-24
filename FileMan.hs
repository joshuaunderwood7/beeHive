module FileMan where
import System.IO

readFileToBuffer :: String -> (String -> b) -> IO b
readFileToBuffer filename inputFunc = do 
            input <- readFile filename
            return $ inputFunc input
            

writeBufferToFile :: String -> String -> IO ()
writeBufferToFile filename output = writeFile  filename output
