module LexicalAnalyzer(split) 

where

import Data.Char
import Data.List



split::[Char] -> [Char] -> [[Char]]
split str terms
    | length inter > 0 = filter (not . null) (let ch = head inter
                                                  first = takeWhile (\e -> e /= ch) str
                                                  others = tail (dropWhile (\e -> e /= ch) str)
                                              in  first : [ch] : (split others terms))
    | otherwise = [str]
    where inter = filter (\e -> e `elem` terms) str