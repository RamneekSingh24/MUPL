import System.IO  
import Control.Monad
import System.Environment   
import Parser
import Typing
import Evaluator



main = do
        args <- getArgs 
        if (length args) == 1 then do
            contents <- readFile (head args)
            putStrLn (show (parse contents))
            putStrLn (show (getType (parse contents)))
            putStrLn (show (eval (parse contents)))
        else do
            print $ "Invalid useage. Please run using ./main <file-name>"
            



