module LexicalAnalyzer
where 
import Data.Char
import Data.List
import Types

convertToMyString :: String -> [MyChar]
convertToMyString str = markAll str 1 1
    where markAll all@(x:xs) curLine curColumn
            | null xs = [(MyChar x (Position curLine curColumn))]
            | x == '\n' = (MyChar '\n' (Position curLine curColumn)) : markAll xs (curLine + 1) 1
            | otherwise = (MyChar x (Position curLine curColumn)) : markAll xs curLine (curColumn + 1)

--{-
possibleLexemes :: [Char] -> [Char] -> [MyWord]
possibleLexemes str terms = splitMy (convertToMyString str) terms


splitMy::[MyChar] -> [Char] -> [MyWord]
splitMy str@(x:xs) terms
    | null str = []
    | p == -1 = [myCharLstToMyWord str]
    | p == 0 = (MyWord [value x] (position x)) : (splitMy (tail str) terms)
    | otherwise = let ch = str !! p 
                  in (myCharLstToMyWord (take (p-1) str)) 
                      : MyWord [value ch] (position ch) 
                      : splitMy (drop (p+1) str) terms
    where
        p = fromJ (findIndex (\e -> (value e) `elem` terms) str)
        fromJ (Just a) = a
        fromJ (Nothing) = -1
---}
myCharLstToMyWord :: [MyChar] -> MyWord
myCharLstToMyWord lst = MyWord (map value lst) (position $ head lst)

--findIndexOrEmpty :: (a -> Bool) -> [a] -> Int
--findIndexOrEmpty f lst = findIndex f lst

