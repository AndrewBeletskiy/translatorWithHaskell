module Main where
import System.IO
import System.Environment
main = do
    args <- getArgs
    let fileName = head args
    print fileName
