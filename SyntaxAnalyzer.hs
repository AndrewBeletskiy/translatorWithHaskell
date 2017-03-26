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
    | and[length opResSnd > 0, code afterOp /= SEMICOLON] = 
        makeSyntaxError (position afterOp) "There must be a semicolon"
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
            then testExpr zs
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
          nextLexCode == MINUS] = testExpr (tail afterTermLst)
    | otherwise = (Nothing, afterTermLst)
    where xCode = (code x)
          termRes = testTerm lst
          afterTermLst = snd termRes
          nextLexCode  = code $ head afterTermLst
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
testIf _ = (Nothing, [])
testDoWhile _ = (Nothing, [])





{-
data LexemeCode = READ|WRITE|IF|THEN
                  |ELSE|DO|WHILE|END|OR
                  |NOT|AND|OPEN_CURLY_BRACKET
                  |CLOSE_CURLY_BRACKET
                  |OPEN_PARENTHESIS
                  |CLOSE_PARANTHESIS
                  |OPEN_BRACKET|CLOSE_BRACKET
                  |PLUS|MINUS|MULTIPLY|DIVIDE
                  |SEMICOLON|COMMA|ASSIGNMENT
                  |EQUAL|MORE|LESS|MORE_OR_EQUAL
                  |LESS_OR_EQUAL|NOT_EQUAL
                  |ID|CONST|STRING
                  |BP|UPL|LABEL|BOOL 
-}