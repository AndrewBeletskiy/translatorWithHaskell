module Main 
where
import LexicalAnalyzer
import System.IO
import System.Environment


iif True x _ = x
iif False _ y = y


main = do
    print "Hello"
    args <- getArgs
    let fileName = head args 
    content <- readFile $ fileName
    let a = lexicalAnalyzer content
    let printed = either f1 f2 a
                     where f1 l = show $ head l
                           f2 r = r
    print printed

