module Main 
where
import LexicalAnalyzer
import Types as TP
import System.IO
import System.Environment


main = do
    print "Hello"
    args <- getArgs
    let fileName = head args 
    content <- readFile $ fileName
    let a = lexicalAnalyzer content
    let toPrint = either f1 f2 a
                     where f1 l = getLexemeTableText l
                           f2 r = show r
    putStrLn toPrint

