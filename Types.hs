module Types
where
-- POSITION TYPE
data Position = Position Int Int
instance Show Position where
    show (Position l c) = "(" ++ (show l) ++ ";" ++ (show c) ++ ")"

nextPosition :: Position -> Position
nextPosition (Position line number) = Position line (number + 1)

prevPosition :: Position -> Position
prevPosition (Position line number) = Position line (number-1)



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
  deriving (Show, Eq)


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
                  deriving (Show, Eq)

data LexemeValueType = StrValue String 
                     | NumValue Double 
                     | LabelValue Int 
                     | BoolValue Bool 
                     | ListValue LexemeValueType LexemeValueType
                     | NoneValue
instance Show LexemeValueType where
  show (StrValue str) = str
  show (NumValue n) = (show n)
  show (LabelValue num) = "m("++(show num)++")"
  show (ListValue l1 l2) = (show l1)++" "++(show l2)

data Lexeme = Lexeme {value :: LexemeValueType, code :: LexemeCode, position :: Position}

instance Show Lexeme where 
    show (Lexeme val code pos) = "\"" ++ (show val) ++ "\" " ++ (show code) ++ (show pos)

getLexemeTableText :: [Lexeme] -> String
getLexemeTableText lexs = unlines $ map show lexs


{--|         GETTERS           |--}
getIntLexemeValue :: Lexeme -> Int
getIntLexemeValue lex = fromEnum $ getDoubleLexemeValue lex

getDoubleLexemeValue :: Lexeme -> Double
getDoubleLexemeValue  (Lexeme (NumValue n) _ _) = n

getStrLexemeValue :: Lexeme -> String
getStrLexemeValue (Lexeme (StrValue str) _ _) = str

getBoolLexemeValue :: Lexeme -> Bool
getBoolLexemeValue (Lexeme (BoolValue b) _ _)=b

getLabelValue :: Lexeme -> Int
getLabelValue (Lexeme (LabelValue num) _ _) = num
  

makeFullLexeme :: LexemeCode -> Position -> Int -> Int -> Lexeme
makeFullLexeme code pos l1 l2
  | code == IF = Lexeme (ListValue (StrValue "if") (ListValue (LabelValue l1) (LabelValue l2))) IF pos
  | code == DO = Lexeme (ListValue (StrValue "do") (ListValue (LabelValue l1) (LabelValue l2))) DO pos
  | otherwise = error "There must be if or do"

makeDoDefault :: Position -> Lexeme
makeDoDefault pos = makeFullLexeme DO pos (-1) (-1)

makeIfDefault :: Position -> Lexeme
makeIfDefault pos = makeFullLexeme IF pos (-1) (-1)

setFstLabelValue :: Lexeme -> Int -> Lexeme
setFstLabelValue lex@(Lexeme val code pos) newVal = changeList val newVal
  where changeList (ListValue main(ListValue (LabelValue _) (LabelValue l2))) newVal = makeFullLexeme code pos newVal l2

setSndLabelValue :: Lexeme -> Int -> Lexeme
setSndLabelValue lex@(Lexeme val code pos) newVal = changeList val newVal
  where changeList (ListValue main(ListValue (LabelValue l1) (LabelValue _))) newVal = makeFullLexeme code pos l1 newVal

getFstLabelValue :: Lexeme  -> Int
getFstLabelValue (Lexeme (ListValue _ (ListValue (LabelValue l1) (LabelValue _))) _ _) = l1

getSndLabelValue :: Lexeme -> Int
getSndLabelValue (Lexeme (ListValue _ (ListValue (LabelValue _) (LabelValue l2))) _ _) = l2

isLabel :: Lexeme
isLabel (Lexeme val code pos) = case lex of (LabelValue _) -> True; otherwise -> False