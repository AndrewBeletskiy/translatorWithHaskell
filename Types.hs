module Types
where

data MyWord = MyWord String Position
instance Show MyWord where
    show (MyWord val pos) = val ++ show pos

data Position = Position {line :: Int, column :: Int}
instance Show Position where
    show (Position l c) = "(" ++ (show l) ++ ";" ++ (show c) ++ ")"

data MyChar = MyChar {value::Char, position::Position}

myChar c i j = MyChar c (Position i j)

instance Show MyChar where
    show (MyChar c pos)
        | c == '\n' = "\\n" ++ show pos
        | otherwise = c : show pos