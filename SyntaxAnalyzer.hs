--SyntaxAnalyzer.hs written by Andrew Beletskiy 2017, March 25
module SyntaxAnalyzer
where
import Types
import Data.Maybe
syntaxAnalyzer :: [Lexeme] -> Maybe String
syntaxAnalyzer [] = Just "Error: There is not any code!"
syntaxAnalyzer lexs = fst (testProg lexs)

makeSyntaxError pos mes = (Just $ "Syntax error in "++(show pos)++": "++mes, [])

testProg :: [Lexeme] -> (Maybe String, [Lexeme])
testProg lst@(x:xs)
    | (code x) /= OPEN_CURLY_BRACKET = makeSyntaxError (position x) "There must be open curly bracket"
    | isJust opListFst = (opListFst, [])
    | null afterOpList = makeSyntaxError lastPos "There must be close curly bracket"
    | (code $ head afterOpList) /= CLOSE_CURLY_BRACKET = makeSyntaxError (position $ head afterOpList) "There must be close curly bracket"
    | length afterOpList > 1 = makeSyntaxError (position $ afterOpList !! 1) "There is some illegal text after close curly bracket"
    | otherwise = (Nothing, [])
    where opListRes = testOpList xs False
          opListFst = (fst opListRes)
          afterOpList = (snd opListRes)
          lastPos = (position $ last lst)

testOpList :: [Lexeme] -> Bool ->(Maybe String, [Lexeme])
testOpList [] _ = (Just "There must be operator list but it didn't", [])
testOpList lst@(x:xs) isWhileLoop
    | isJust opResFst = opRes
    | null opResSnd = makeSyntaxError (nextPosition $ position $ last lst) "There must be ';'"
    | code afterOp /= SEMICOLON = 
        makeSyntaxError (position afterOp) "There must be ';'"
    | length opResSnd < 2 = (Nothing, opResSnd)
    | code afterOp /= SEMICOLON = makeSyntaxError (position afterOp) "There must be a semicolon after operator"
    | or [and[isWhileLoop, (code (opResSnd !! 1)) == END], 
          and[not isWhileLoop, (code (opResSnd !! 1))== CLOSE_CURLY_BRACKET]] = (Nothing, tail opResSnd)
    | otherwise = testOpList (tail opResSnd) isWhileLoop
    where opRes = testOp lst
          opResFst = fst opRes
          opResSnd = snd opRes
          afterOp = head opResSnd
          lastPos = (position $ last lst)

testOp :: [Lexeme] -> (Maybe String, [Lexeme])
testOp [] = (Just "There must be operator but it didn't", [])
testOp lst@(x:_)
    | xCode == ID = testAssignment lst
    | xCode == READ = testRead lst
    | xCode == WRITE = testWrite lst
    | xCode == IF = testIf lst
    | xCode == DO = testDoWhile lst
    | otherwise = makeSyntaxError (position x) "There must be 'id' or 'write' or 'read' or 'if' or 'do'"
    where xCode = (code x)

testAssignment :: [Lexeme] -> (Maybe String, [Lexeme])
testAssignment [] = (Just "There must be assignment operator",[])
testAssignment lst = case lst of
    (x:y:zs) ->
        if (and[xCode == ID, yCode == ASSIGNMENT]) 
            then if null zs then makeSyntaxError (nextPosition $ position y) "There must be expression."
                            else testExpr zs
            else makeSyntaxError (position $ head lst) "Illegal assignment expression"
        where xCode = (code x)
              yCode = (code y)
    otherwise -> makeSyntaxError (position $ head lst) "Illegal assignment expression"

testExpr :: [Lexeme] -> (Maybe String, [Lexeme])
testExpr [] = (Just "There must be expression!", [])
testExpr lst@(x:xs)
    | and[xCode /= ID, 
          xCode /= CONST, 
          xCode /= OPEN_PARENTHESIS] 
      = makeSyntaxError (position x) $ "Illegal expression. It must starts with 'id' or '(' or 'const'. Actual " ++ (show xCode)
    | not isTerm = termRes
    | null afterTermLst = (Nothing, [])
    | or [nextLexCode == PLUS, 
          nextLexCode == MINUS] = if (null (tail afterTermLst)) 
                                  then makeSyntaxError (nextPosition $ position nextLex) "There must be expression"
                                  else testExpr (tail afterTermLst)
    | otherwise = (Nothing, afterTermLst)
    where xCode = (code x)
          termRes = testTerm lst
          afterTermLst = snd termRes
          nextLex = head afterTermLst
          nextLexCode  = code nextLex
          isTerm = isNothing (fst termRes)

testTerm :: [Lexeme] -> (Maybe String, [Lexeme])
testTerm lst@(x:xs)
    | and[xCode /= ID, 
          xCode /= CONST, 
          xCode /= OPEN_PARENTHESIS] 
      = makeSyntaxError (position x) ("Illegal term. It must starts with 'id' or '(' or 'const'. Actual " ++ (show xCode))
    | not isMnoj = mnojRes
    | null afterMnojLst = (Nothing, [])
    | or [nextLexCode == MULTIPLY, 
          nextLexCode == DIVIDE] = testTerm (tail afterMnojLst)
    | otherwise = (Nothing, afterMnojLst)
    where xCode = (code x)
          mnojRes = testMnoj lst
          afterMnojLst = snd mnojRes
          nextLexCode  = code $ head afterMnojLst
          isMnoj = isNothing (fst mnojRes)

testMnoj :: [Lexeme] -> (Maybe String, [Lexeme])
testMnoj [] = (Just "There must be assignment operator",[])
testMnoj lst@(x:xs)
    | or[ xCode == ID 
        , xCode == CONST
        ] = (Nothing, xs)
    | xCode == OPEN_PARENTHESIS = 
        let exprRes = testExpr xs
            afterExpr = snd exprRes
            isExpr = isNothing (fst exprRes)
        in if (and[isExpr, (length afterExpr) > 0, (code (head afterExpr)) == CLOSE_PARANTHESIS])
           then (Nothing, tail afterExpr)
           else if (isExpr) then makeSyntaxError (position $ head afterExpr) "There must be ')'"
                            else exprRes
    where xCode = (code x)

testRead :: [Lexeme] -> (Maybe String, [Lexeme])
testRead lst@(x:y:zs)
    | xCode /= READ = makeSyntaxError posx ("There must be read statement!. Actual " ++ (show x))
    | yCode /= OPEN_PARENTHESIS = makeSyntaxError posy ("There must be '('. Actual " ++ (show y))
    | isJust (fst testIds) = testIds
    | null afterLst = makeSyntaxError (position $ head zs) "There must be ')'. Actua - Nothing"
    | afterCode /= CLOSE_PARANTHESIS = makeSyntaxError (posClose) ("There must be ')'. Actual " ++ (show afterCode))
    | otherwise = (Nothing, tail afterLst)
    where xCode = code x
          yCode = code y
          posy = position y
          posx = position x
          testIds = testIdList zs
          afterLst = (snd testIds)
          aft = head afterLst
          afterCode = code aft
          posClose = position aft
testIdList :: [Lexeme] -> (Maybe String, [Lexeme])
testIdList (x:y:xs)
    | xCode /= ID = makeSyntaxError posx ("There must be id, but there is " ++ (show x))
    | yCode /= COMMA = (Nothing, y:xs)
    | yCode == COMMA = testIdList xs
    where xCode = code x
          yCode = code y
          posx =position x

testWrite :: [Lexeme] -> (Maybe String, [Lexeme])
testWrite lst@(x:y:zs)
    | xCode /= WRITE = makeSyntaxError posx ("There must be write statement!. Actual " ++ (show x))
    | yCode /= OPEN_PARENTHESIS = makeSyntaxError posy ("There must be '('. Actual " ++ (show y))
    | isJust (fst testStrIdLst) = testStrIdLst
    | null afterLst = makeSyntaxError (position $ head zs) "There must be ')'. Actual - Nothing"
    | afterCode /= CLOSE_PARANTHESIS = makeSyntaxError (posClose) ("There must be ')'. Actual " ++ (show afterCode))
    | otherwise = (Nothing, tail afterLst)
    where xCode = code x
          yCode = code y
          posy = position y
          posx = position x
          testStrIdLst = testIdStringList zs
          afterLst = (snd testStrIdLst)
          aft = head afterLst
          afterCode = code aft
          posClose = position aft


testIdStringList :: [Lexeme] -> (Maybe String, [Lexeme])
testIdStringList (x:y:xs)
    | and[xCode /= ID, xCode /= STRING] = makeSyntaxError posx ("There must be id or string, but there is " ++ (show x))
    | yCode /= COMMA = (Nothing, y:xs)
    | yCode == COMMA = testIdStringList xs
    where xCode = code x
          yCode = code y
          posx =position x

testLogExpr :: [Lexeme] -> (Maybe String, [Lexeme])
testLogExpr [] = (Just "There must be expression!", [])
testLogExpr lst@(x:xs)
    | and[xCode /= ID, 
          xCode /= CONST, 
          xCode /= OPEN_BRACKET,
          xCode /= NOT,
          xCode /= OPEN_PARENTHESIS] 
      = makeSyntaxError (position x) $ "Illegal expression. It must starts with 'id' or '[' or 'const'. Actual " ++ (show xCode)
    | not isLogTerm = logTermRes
    | null afterLogTermLst = (Nothing, [])
    | nextLexCode == OR = testLogExpr (tail afterLogTermLst)
    | otherwise = (Nothing, afterLogTermLst)
    where xCode = (code x)
          logTermRes = testLogTerm lst
          afterLogTermLst = snd logTermRes
          nextLexCode  = code $ head afterLogTermLst
          isLogTerm = isNothing (fst logTermRes)


testLogTerm :: [Lexeme] -> (Maybe String, [Lexeme])
testLogTerm [] = (Just "There must be logical term",[])
testLogTerm lst@(x:xs)
    | and[xCode /= ID, 
          xCode /= CONST, 
          xCode /= OPEN_BRACKET,
          xCode /= NOT,
          xCode /= OPEN_PARENTHESIS]  
      = makeSyntaxError (position x) ("Illegal term. It must starts with 'id' or '[' or 'const'. Actual " ++ (show xCode))
    | not isLogMnoj = logMnojRes
    | null afterMnojLst = (Nothing, [])
    | nextLexCode == AND = testLogTerm (tail afterMnojLst)
    | otherwise = (Nothing, afterMnojLst)
    where xCode = (code x)
          logMnojRes = testLogMnoj lst
          afterMnojLst = snd logMnojRes
          nextLexCode  = code $ head afterMnojLst
          isLogMnoj = isNothing (fst logMnojRes)

testLogMnoj :: [Lexeme] -> (Maybe String, [Lexeme])
testLogMnoj [] = (Just "There must be assignment operator",[])
testLogMnoj lst@(x:xs)
    | xCode == OPEN_BRACKET = 
        let exprRes = testLogExpr xs
            afterExpr = snd exprRes
            isExpr = isNothing (fst exprRes)
        in if (and[isExpr, (length afterExpr) > 0, (code (head afterExpr)) == CLOSE_BRACKET])
           then (Nothing, tail afterExpr)
           else if (isExpr && not (null afterExpr))
                then makeSyntaxError (position $ head afterExpr) "There must be ']'"
                else if (null afterExpr) 
                     then makeSyntaxError (position x) "There must be ']'"
                     else exprRes
    | xCode == NOT = testLogTerm xs
    | otherwise = testRelation (x:xs)
    where xCode = (code x)

testRelation :: [Lexeme] -> (Maybe String, [Lexeme])
testRelation [] = (Just "There must be a relation statement.", [])
testRelation lst@(x:xs)
    | not isFirstExpr = fstExprRes
    | length aftFstExpr < 2 = makeSyntaxError (position x) "There must be a relation statement"
    | not isRelSign = makeSyntaxError (position relLex) "There must be a relation sign such as '>', '<', '<=' '>=' '=' '!='"
    | not isSndExpr = sndExprRes
    | otherwise = (Nothing, aftSndExpr)
    where fstExprRes = testExpr lst
          aftFstExpr = (snd fstExprRes)
          relLex = head aftFstExpr
          relCode = code relLex
          isRelSign = or[ relCode == MORE
                        , relCode == LESS
                        , relCode == MORE_OR_EQUAL
                        , relCode == LESS_OR_EQUAL
                        , relCode == EQUAL
                        , relCode == NOT_EQUAL
                        ]
          sndExprRes = testExpr (tail aftFstExpr)
          aftSndExpr = snd sndExprRes
          isFirstExpr = isNothing (fst fstExprRes)
          isSndExpr = isNothing (fst sndExprRes)

testIf :: [Lexeme] -> (Maybe String, [Lexeme])
testIf [] = (Just "There must be if statement.", [])
testIf lst@(x:xs)
    | xCode /= IF = makeSyntaxError posx "There must be 'if'"
    | null xs = makeSyntaxError posx "There must be a logical expression"
    | not isLogExpr = logExprRes
    | null aftLogExpr = makeSyntaxError (position $ last xs) "There must be a 'then'"
    | mbThenCode /= THEN = makeSyntaxError (position mbThenLex) "There must be 'then'"
    | null aftThen = makeSyntaxError (nextPosition $ position mbThenLex) "There must be operator"
    | not isFstOp = fstOpRes
    | null aftFstOp = makeSyntaxError (nextPosition $ position $ last aftThen) "There must be 'else'"
    | mbElseCode /= ELSE = makeSyntaxError (position mbElseLex) "There must be 'else'"
    | null aftElse = makeSyntaxError (nextPosition $ position mbElseLex) "There must be operator"
    | not isSndOp = sndOpRes
    | otherwise = (Nothing, (snd sndOpRes))
    where aftElse = tail aftFstOp
          aftFstOp = snd fstOpRes
          aftLogExpr = snd logExprRes
          aftThen = tail aftLogExpr
          fstOpRes = testOp aftThen
          sndOpRes = testOp aftElse
          isFstOp = isNothing (fst fstOpRes)
          isLogExpr = isNothing (fst logExprRes)
          isSndOp = isNothing (fst sndOpRes)
          logExprRes = testLogExpr xs
          mbElseCode = code mbElseLex
          mbElseLex = head aftFstOp
          mbThenCode = code mbThenLex
          mbThenLex = head aftLogExpr
          posx = position x
          xCode = code x

testDoWhile :: [Lexeme] -> (Maybe String, [Lexeme])
testDoWhile [] = (Just "There must be 'do'", [])
testDoWhile [x]
  | (code x) == DO = makeSyntaxError (nextPosition $ position x) "There must be 'while'"
  | otherwise = makeSyntaxError (position x) "There must be 'do'"

testDoWhile (mbDo:mbWhile:mbPar:aftPar)
  | mbDoCode /= DO = makeSyntaxError (position mbDo) "There must be 'do'"
  | mbWhileCode /= WHILE = makeSyntaxError (position mbWhile) "There must be 'while'"
  | code mbPar /= OPEN_PARENTHESIS = makeSyntaxError (position mbPar) "There must be '('"
  | null aftPar = makeSyntaxError (nextPosition $ position mbPar) "There must be a logical expression"
  | not isLogExpr = logExprRes--makeSyntaxError (position $ head aftPar) "There must be a logical expression"
  | ((length aftLog) < 2) = makeSyntaxError (nextPosition  $ position $ last aftPar) "There must be ');'"
  | mbClParCode /= CLOSE_PARANTHESIS = makeSyntaxError (position mbClPar) "There must be ')'"
  | mbSemiCode /= SEMICOLON = makeSyntaxError (position mbSemi) "There must be ';'"
  | null aftSemi = makeSyntaxError (nextPosition $ position mbSemi) "There must be list of operators"
  | not isOpList = opListRes--makeSyntaxError (position $ head aftSemi) "There must be list of operators"
  | null aftOpList = makeSyntaxError (nextPosition $ position $ last aftSemi) "There must be 'end'"
  | mbEndCode /= END = makeSyntaxError (position $ head aftOpList) "There must be 'end'"
  | otherwise = (Nothing, aftEnd)
  where
    mbDoCode = code mbDo
    posDo = position mbDo
    mbWhileCode = code mbWhile
    posWhile = position mbWhile
    logExprRes = testLogExpr aftPar
    isLogExpr = isNothing (fst logExprRes)
    aftLog = (snd logExprRes)
    mbClPar = head aftLog
    mbClParCode = code mbClPar
    mbSemi = aftLog !! 1
    mbSemiCode = code mbSemi
    aftSemi = drop 2 aftLog
    opListRes = testOpList aftSemi True 
    isOpList = isNothing (fst opListRes)
    aftOpList = snd opListRes
    mbEndCode = code $ head aftOpList
    aftEnd = tail aftOpList