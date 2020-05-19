module Helpers (
    fst3,
    swapElementsAt,
    unicodeFromHex,
    hexChar,
    parseHex
) where
    
import Data.Char (chr, ord, intToDigit, toUpper)


fst3:: (a, b, c) -> a
fst3 (a, _, _) = a

swapElementsAt :: Int -> Int -> [a] -> [a]
swapElementsAt a b list 
    | a > b     = swapElementsAt b a list
    | otherwise = list1 ++ [list !! b] ++ list2 ++ [list !! a] ++ list3
        where   list1 = take a list;
                list2 = drop (succ a) (take b list);
                list3 = drop (succ b) list

unicodeFromHex :: String -> String
unicodeFromHex hexString = map chr $ hextoDec $ map toUpper hexString
    where
        hextoDec:: String -> [(Int)]
        hextoDec []        = []
        hextoDec hexString = 
            let (x:y:xs) = hexString 
            in (parseHex ([x] ++ [y])) : hextoDec xs
                
hexChar :: Char -> Int
hexChar ch
    | ch == '0' = 0
    | ch == '1' = 1
    | ch == '2' = 2
    | ch == '3' = 3
    | ch == '4' = 4
    | ch == '5' = 5
    | ch == '6' = 6
    | ch == '7' = 7
    | ch == '8' = 8
    | ch == '9' = 9
    | ch == 'A' = 10
    | ch == 'B' = 11
    | ch == 'C' = 12
    | ch == 'D' = 13
    | ch == 'E' = 14
    | ch == 'F' = 15
    | otherwise     = 0

parseHex :: String -> Int
parseHex hxStr 
    | length hxStr /= 0 = (hexChar(last(hxStr)))+(16*parseHex(init(hxStr)))
    | otherwise         = 0  