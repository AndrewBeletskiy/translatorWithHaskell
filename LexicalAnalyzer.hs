module LexicalAnalyzer
where 
import Data.Char
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
        | cls == CharClassError = makeError val pos "BAD CHARACTER!"
        | state == 1 = case cls of 
          CharClassLetter ->    (2, val:curStr, res)
          CharClassDigit ->     (3, val:curStr, res)
          CharClassPoint ->     (4, val:'0':curStr, res)
          CharClassQuote ->     (9, curStr, res)
          CharClassDelimiter -> (1, [], newLex:res)
            where 
              lexCode = findLexemeCodeOrID [val]
              newLex = Lexeme (StrValue [val]) lexCode pos
          CharClassColon ->     (10, val:curStr, res)
          CharClassEqual -> (1, [], (newLex:res))
            where newLex = Lexeme (StrValue [val]) EQUAL pos
          CharClassMore ->      (11, val:curStr, res)
          CharClassLess ->      (12, val:curStr, res)
          CharClassExclamationPoint -> (13, val:curStr, res)
          CharClassEmpty ->     (1, [], res)
          otherwise ->          makeError val pos "BAD CHARACTER!"
        | state == 2 = case cls of 
          CharClassLetter -> (2, val:curStr, res)
          CharClassDigit ->  (2, val:curStr, res)
          otherwise -> changeState chr 1 [] (newLex:res)
            where revS = reverse curStr
                  lexCode = findLexemeCodeOrID revS
                  newLex = Lexeme (StrValue revS) lexCode pos
        | and [state == 3
              ,or[val == 'e'
                 ,val == 'E'
                 ]
              ] = (6, val:curStr, res)
        | state == 3 = case cls of 
          CharClassDigit -> (3, val:curStr, res)
          CharClassPoint -> (5, val:curStr, res)
          otherwise -> changeState chr 1 [] (newLex:res)
            where
              revS = reverse curStr
              lexVal = NumValue ((read revS) :: Double)
              newLex = Lexeme lexVal CONST pos
        | state == 4 = case cls of
          CharClassDigit -> (5, val:curStr, res)
          otherwise  -> makeError val pos "Must be a digit after point."
        | and [state == 5
              ,or[val == 'e'
                 ,val == 'E'
                 ]
              ] = (6, val:'0':curStr, res)
        | and[state == 5, cls == CharClassDigit] = (5, val:curStr, res)
        | and[state == 5, (head curStr) == '.'] = changeState chr 1 [] 
                         $ (Lexeme (NumValue ((read $ reverse ('0':curStr))::Double))
                                   CONST
                                   pos):res
        | state == 5 =
            (changeState chr 1 [] 
                         $ (Lexeme (NumValue ((read $ reverse curStr)::Double))
                                   CONST
                                   pos):res)
        | and[state == 6
             , or [val == '+'
                  ,val == '-']
             ] = (7, val:curStr, res)
        | and[state==6, cls == CharClassDigit] = (8, val:curStr, res)
        | state == 6 = makeError val pos "Must be a number or a sign after E."
        | and[state==7, cls == CharClassDigit] = (8, val:curStr, res)
        | state == 7 = makeError val pos "Must be number after the sign in a number constant."
        | and[state==8, cls == CharClassDigit] = (8, val:curStr, res)
        | state == 8 = 
            (changeState chr 1 [] 
                         $ (Lexeme (NumValue ((read $ reverse curStr)::Double))
                                   CONST
                                   pos):res)
        | and[state==9, val /= '\"'] = (9, val:curStr, res)
        | and[state==9, val == '\"'] =
            (1, [], (Lexeme (StrValue (replaceEscape $ reverse curStr)) STRING pos):res)
        | and[state==10, val=='='] = (1, [], (Lexeme (StrValue ":=") ASSIGNMENT pos):res)
        | state==10 = makeError val pos "Must be sign '=' after ':'."
        | and[state==11, val=='='] = (1, [], (Lexeme (StrValue ">=") MORE_OR_EQUAL pos):res)
        | state==11 = (changeState chr 1 [] 
                         $ (Lexeme (StrValue ">") MORE pos):res)
        | and[state==12, val=='='] = (1, [], (Lexeme (StrValue "<=") LESS_OR_EQUAL pos):res)
        | state==12 = (changeState chr 1 [] 
                         $ (Lexeme (StrValue "<") LESS pos):res)
        | and[state==13, val=='='] = (1, [], (Lexeme (StrValue "!=") NOT_EQUAL pos):res)
        | state==13 = makeError val pos "Here must be '='."
        | otherwise = (-1, "There is no such situation in automat!"
                            ++(show state)++" "++(show chr), [])
        where cls = getCharClass chr

    makeError val pos mes = (-1, "Illegal character '"
                             ++[val]++"' on " 
                             ++ (show pos)++". "++mes,[])
    thrd' (_,_,x) = x
    snd' (_,x,_) = x
    fst' (x,_,_) = x
    replaceEscape [] = []
    replaceEscape (x:[]) = [x]
    replaceEscape (x:y:xs)
      | and[x == '\\', y == 'n'] = '\n' : replaceEscape xs
      | and[x == '\\', y == 't'] = '\t' : replaceEscape xs
      | and[x == '\\', y == '\\'] = '\\' : replaceEscape xs
      | otherwise = x:replaceEscape (y:xs)

findLexemeCodeOrID :: [Char] -> LexemeCode
findLexemeCodeOrID str = fromMaybe ID $ lookup str strLexemeCodes

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