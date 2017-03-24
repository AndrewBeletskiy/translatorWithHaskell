module LexicalAnalyzer
where 
import Data.Char
import Data.List
import Types

lexicalAnalyzer :: String -> Either [Lexeme] String
lexicalAnalyzer text = f (convertToMyString text) 1 [] []
    where 
        f [] state curStr res
            | state < 0 = Right curStr
            | state == 1 = Left (reverse res)
            | otherwise = Right ("Error! Unexpected end of file. Current string: '"++ curStr ++ "'")
        f code@(x:xs) state curStr res
            | state < 0 = Right curStr
            | otherwise = let nextStateStrRes = changeState x state curStr res  
                          in f xs (fst' nextStateStrRes) (snd' nextStateStrRes) (thrd' nextStateStrRes)
        changeState chr state curStr res
            | otherwise = (-1, "There is no such situation in automat!", [])
            where cls = getCharClass chr
        thrd' (_,_,x) = x
        snd' (_,x,_) = x
        fst' (x,_,_) = x

getCharClass (MyChar val _)
            | val `elem` " \n\r\t" = CharClassEmpty
            | or[and[val >= 'a', val <='z'],and[val >= 'a', val <='z']] = CharClassLetter
            | isDigit val = CharClassDigit
            | val `elem` "{}()[]+-*/;,\\" = CharClassDelimiter
            | val == ':' = CharClassColon
            | val == '=' = CharClassEqual
            | val == '>' = CharClassMore 
            | val == '<' = CharClassLess
            | val == '!' = CharClassExclamationPoint
            | val == '\"' = CharClassQuote
            | val == '.' = CharClassPoint
            | otherwise = CharClassError



-- state == 1 = (1, [], (Lexeme curStr BOOL (charPos chr) Nothing) : res)