import System.IO  
import Control.Monad
import System.Environment   
import Parser
import Typing




main = do
        args <- getArgs 
        if (length args) == 1 then do
            contents <- readFile (head args)
            putStrLn (show (parse contents))
            putStr (show (getType (parse contents)))
        else do
            print $ "Invalid useage. Please run using ./a2 <file-name>"
            



