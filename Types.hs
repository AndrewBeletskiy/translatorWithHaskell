module Types
where
-- POSITION TYPE
data Position = Position Int Int
instance Show Position where
    show (Position l c) = "(" ++ (show l) ++ ";" ++ (show c) ++ ")"


-- Char with position TYPE
data MyChar = MyChar {charVal::Char, charPos::Position}
instance Show MyChar where
    show (MyChar c pos)
        | c == '\n' = "\\n" ++ show pos
        | otherwise = c : show pos
myChar c i j = MyChar c (Position i j)

convertToMyString :: String -> [MyChar]
convertToMyString str = markAll str 1 1
    where markAll [] _ _ = []
          markAll all@(x:xs) curLine curColumn
            | null xs = [(MyChar x (Position curLine curColumn))]
            | x == '\n' = (MyChar '\n' (Position curLine curColumn)) : markAll xs (curLine + 1) 1
            | otherwise = (MyChar x (Position curLine curColumn)) : markAll xs curLine (curColumn + 1)

data MyWord = MyWord String Position
instance Show MyWord where
    show (MyWord val pos) = val ++ show pos

myCharLstToMyWord :: [MyChar] -> MyWord
myCharLstToMyWord lst = MyWord (map charVal lst) (charPos $ head lst)

data CharClass = CharClassColon|CharClassDelimiter|CharClassDigit|CharClassEmpty|CharClassEqual|CharClassError|CharClassExclamationPoint|CharClassLess|CharClassLetter|CharClassMore|CharClassPoint|CharClassQuote
  deriving (Show)


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
                  deriving (Show)

data Lexeme = Lexeme {value :: String, code :: LexemeCode, position :: Position, classNumber :: Maybe Int}
instance Show Lexeme where 
    show (Lexeme val code pos classNumber)
        | (classNumber == Nothing) = "\"" ++ val ++ "\" " ++ (show code) ++ (show pos)
        | otherwise = "\"" ++ val ++ "\" " ++ (show code) ++ (show pos) ++ " Ind: " ++ (show $ fromJ classNumber)
        where 
            fromJ (Just a) = a
            fromJ Nothing = -1;