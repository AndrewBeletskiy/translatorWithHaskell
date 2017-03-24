module Main 
where
import LexicalAnalyzer
import System.IO
import System.Environment

main = do
    args <- getArgs
    content <- readFile $ head args
    let pos = possibleLexemes content " "

    print (map show pos)
