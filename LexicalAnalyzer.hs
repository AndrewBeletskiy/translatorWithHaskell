module LexicalAnalyzer
where 
import Data.Char
import Data.List
import Data.Maybe
import Types

lexicalAnalyzer :: String -> Either [Lexeme] String
lexicalAnalyzer text = f (convertToMyString $ text++" ") 1 [] []
  where
    f [] state curStr res
        | state < 0 = Right curStr
        | state == 1 = Left (reverse res)
        | otherwise = Right ("Error! Unexpected end of file. S: "
                             ++(show state)
                             ++". Current string: '"
                             ++ curStr ++ "'")
    f code@(x:xs) state curStr res
        | state < 0 = Right curStr
        | otherwise = 
            let nextStateStrRes = changeState x state curStr res
            in f xs (fst' nextStateStrRes) 
                    (snd' nextStateStrRes) 
                    (thrd' nextStateStrRes)
    changeState chr@(MyChar val pos) state curStr res
        | state == 1 = case cls of 
          CharClassLetter ->    (2, val:curStr, res)
          CharClassDigit ->     (3, val:curStr, res)
          CharClassPoint ->     (4, val:curStr, res)
          CharClassQuote ->     (9, val:curStr, res)
          CharClassDelimiter -> (1, [], (Lexeme [val] (findLexemCodeOrID [val]) pos):res)
          CharClassColon ->     (10, val:curStr, res)
          CharClassEqual ->     (1, [], (Lexeme [val] EQUAL pos):res)
          CharClassMore ->      (11, val:curStr, res)
          CharClassLess ->      (12, val:curStr, res)
          CharClassExclamationPoint -> (13, val:curStr, res)
          CharClassEmpty ->     (1, [], res)
          otherwise ->          (-1, "Illegal character '"
                                     ++[val]++"' on " 
                                     ++ (show pos), [])
        | state == 2 = case cls of CharClassLetter -> (2, val:curStr, res)
                                   CharClassDigit -> (2, val:curStr, res)
                                   otherwise -> (changeState chr 1 [] 
                                                 $ (Lexeme (reverse curStr) 
                                                           (findLexemCodeOrID $ reverse curStr) 
                                                           pos):res)
        | cls == CharClassError = (-1, "Illegal character '"
                                       ++[val]++"' on " 
                                       ++ (show pos), [])
        | otherwise = (-1, "There is no such situation in automat!"++(show state)++" "++(show chr), [])
        where cls = getCharClass chr
    thrd' (_,_,x) = x
    snd' (_,x,_) = x
    fst' (x,_,_) = x

findLexemCodeOrID :: [Char] -> LexemeCode
findLexemCodeOrID str = fromMaybe ID $ lookup str strLexemeCodes

strLexemeCodes :: [([Char], LexemeCode)]
strLexemeCodes = [("read",READ), 
                  ("write",WRITE),
                  ("if",IF),
                  ("then",THEN),
                  ("else",ELSE),
                  ("do",DO),
                  ("while",WHILE),
                  ("end",END),
                  ("or",OR),
                  ("not",NOT),
                  ("and",AND),
                  ("{",OPEN_CURLY_BRACKET),
                  ("}",CLOSE_CURLY_BRACKET),
                  ("(",OPEN_PARENTHESIS),
                  (")",CLOSE_PARANTHESIS),
                  ("[",OPEN_BRACKET),
                  ("]",CLOSE_BRACKET),
                  ("+",PLUS),
                  ("-",MINUS),
                  ("*",MULTIPLY),
                  ("/",DIVIDE),
                  (";",SEMICOLON),
                  (",",COMMA),
                  (":=",ASSIGNMENT),
                  ("=",EQUAL),
                  (">",MORE),
                  ("<",LESS),
                  (">=",MORE_OR_EQUAL),
                  ("<=",LESS_OR_EQUAL),
                  ("!=",NOT_EQUAL)]

getCharClass :: MyChar -> CharClass
getCharClass (MyChar val _)
            | val `elem` " \n\r\t" = CharClassEmpty
            | or[and[val >= 'a', val <='z'],and[val >= 'A', val <='Z']] = CharClassLetter
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



