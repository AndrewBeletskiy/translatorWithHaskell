module Main 
where
import Data.Either
import Data.Maybe
import LexicalAnalyzer
import SyntaxAnalyzer
import System.Environment
import System.IO
import Types as TP


main = do
    print "Hello"
    {-
    args <- getArgs
    let fileName = head args
    content <- readFile $ fileName
     -}
    content <- readFile "test.txt"
    let a = lexicalAnalyzer content
    let toPrint = maybe "OK" show (synt content)
    putStrLn toPrint

testProgram [] = False
testProgram text
    | isRight lexRes = False
    | isJust $ syntaxAnalyzer $ fromLeft lexRes = False
    | otherwise = True
    where lexRes = lexicalAnalyzer text

fromLeft x= (lefts [x]) !! 0
fromRight x= (rights [x]) !! 0

synt :: [Char] -> Maybe String
synt text
    | isRight lexRes = Just $ fromRight lexRes
    | otherwise = syntaxAnalyzer $ fromLeft lexRes
    where lexRes = lexicalAnalyzer text
