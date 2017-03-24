module LexicalAnalyzer(split) 

where

import Data.Char
import Data.List

data Position = Position {line :: Int, column :: Int}
instance Show Position where
    show (Position l c) = "(" ++ (show l) ++ ";" ++ (show c) ++ ")"

data MyChar = MyChar {value::Char, position::Position}

myChar c i j = MyChar c (Position i j)

instance Show MyChar where
    show (MyChar c pos)
        | c == '\n' = "\\n" ++ show pos
        | otherwise = c : show pos


type MyString = [MyChar]

convertToMyString :: String -> MyString
convertToMyString str = markAll str 1 1
    where markAll all@(x:xs) curLine curColumn
            | null xs = [(MyChar x (Position curLine curColumn))]
            | x == '\n' = (MyChar '\n' (Position curLine curColumn)) : markAll xs (curLine + 1) 1
            | otherwise = (MyChar x (Position curLine curColumn)) : markAll xs curLine (curColumn + 1)


split::[Char] -> [Char] -> [[Char]]
split str terms
    | length inter > 0 = filter (not . null) (let ch = head inter
                                                  first = takeWhile (\e -> e /= ch) str
                                                  others = tail (dropWhile (\e -> e /= ch) str)
                                              in  first : [ch] : (split others terms))
    | otherwise = [str]
    where inter = filter (\e -> e `elem` terms) str

{-split::[Char] -> [Char] -> [[Char]]
split str terms
    | length inter > 0 = filter (not . null) (let ch = head inter
                                                  first = takeWhile (\e -> e /= ch) str
                                                  others = tail (dropWhile (\e -> e /= ch) str)
                                              in  first : [ch] : (split others terms))
    | otherwise = [str]
    where inter = filter (\e -> e `elem` terms) str-}