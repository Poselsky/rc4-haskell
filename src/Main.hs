module Main where
import Data.List
import Data.Char (chr, ord, intToDigit, toUpper)
import Data.Bits (xor)
import Text.Read
import Data.Maybe
import Numeric


main :: IO()
main = do
    let mainFunctions = [encryptIO, decryptIO, exitProgram]
    input <- mapM_ putStrLn ["Please choose an option (1. Default):", "1. Encrypt message", "2. Decrypt message", "3. Exit application"] 
            >>= return(getLine)
            >>= (\x -> return (readMaybe x :: Maybe Int))
            >>= (\x -> case x of    Just x    -> if x <= length mainFunctions && x > 0 then return(Just x) else return(Nothing)
                                    otherwise -> return(Just 1))
    
    case input of   Nothing      -> do 
                                putStrLn "Please, choose correct item from menu."
                                main
                    -- -1 is because of menu selection, in Haskell, indexing starts at 0
                    Just input -> mainFunctions !! (input - 1)
                    
    return()

encryptIO:: IO()
encryptIO = do
    (message, key) <- putStrLn "Write message you want to encrypt" 
            >>= return(getLine)
            -- Get encryption key and with message return tuple
            >>= (\message -> do
                    putStrLn "Write encryption key"
                    key <- getLine
                    return(message, key))
    let encryptedMessage = encryptRC4 message key
    putStrLn $ "This is your encrypted message: " ++ encryptedMessage
    
    shouldContinueProgram <- shouldContinue
    if shouldContinueProgram then main else return()

decryptIO:: IO()
decryptIO = do
    (message, key) <- putStrLn "Write message you want to decrypt" 
            >>= return(getLine)
            -- Get encryption key and with message return tuple
            >>= (\message -> do
                    putStrLn "Write encryption key"
                    key <- getLine
                    return(message, key))
    let decryptedMessage = decryptRC4 message key
    putStrLn $ "This is your decrypted message: " ++ decryptedMessage
    
    shouldContinueProgram <- shouldContinue
    if shouldContinueProgram then main else return()

exitProgram:: IO()
exitProgram = do
    return()


shouldContinue:: IO(Bool)
shouldContinue = do 
    input <- putStrLn "Do you wish to continue? [y, N]" 
        >>= (\_ -> do
                yesNo <- getLine
                if (toUpper $ head yesNo) == 'Y' then return(True) else return(False))
    return(input)

{-********************
    MAIN LOGIC
*********************-}
type ModifiedIndexList = (Int, [Int])
type Key = String
type Keystream = [Int]

ksa:: Key -> Keystream
ksa key = calculateJWithSwap 0 (map ord key) (0, [0..255])
    where
        calculateJWithSwap:: Int -> [Int] -> ModifiedIndexList -> Keystream
        calculateJWithSwap numberOfIterations key (previousIndexJ, keyList) = 
            if numberOfIterations < 256
                then calculateJWithSwap (numberOfIterations + 1) key (indexJ, swappedValues)
            else keyList
                where 
                    keyLength = length key
                    indexJ = (previousIndexJ + (keyList !! numberOfIterations) + (key !! (numberOfIterations `mod` keyLength))) `mod` 256
                    swappedValues = swapElementsAt numberOfIterations indexJ keyList


-- There is reverse to match the standard
prga:: Int -> Keystream -> [Int]
prga numberOfIterations keyStream = reverse . map fst3 $ init $ indexCalcHelper keyStream numberOfIterations [(0,0,0)]
    where
        indexCalcHelper:: Keystream -> Int -> [(Int,Int,Int)] -> [(Int,Int,Int)]
        indexCalcHelper modifiedKeyStream numberOfIterations acc
            | numberOfIterations == 0 = acc
            | otherwise               = indexCalcHelper swapped (numberOfIterations-1) ((indexK,indexI,indexJ):acc)
                where
                    indexI = (i + 1) `mod` 256
                    indexJ = (j + (modifiedKeyStream !! indexI)) `mod` 256
                    swapped = swapElementsAt indexI indexJ modifiedKeyStream
                    indexK =  swapped !! (((swapped !! indexI) + (swapped !! indexJ)) `mod` 256)
                    ((k,i,j):xs) = acc


encryptRC4:: String -> Key -> String
encryptRC4 message key = inHex
    where
        messageLength = length message
        keyStream = ksa key
        xored = map (\(messageChar, intFromKeyStream) -> (intFromKeyStream `xor` (ord messageChar))) $ zip message $ prga messageLength keyStream
        inHex = map toUpper $ concatMap toHexMapHelper xored
            where 
                -- I need two chars, if results to 0 -> it won't append 0 to the results
                toHexMapHelper:: Int -> String
                toHexMapHelper x = 
                    let hex = showIntAtBase 16 intToDigit x ""
                    in 
                        if length hex `mod` 2 == 0 then hex
                        else '0':hex

decryptRC4:: String -> Key -> String 
decryptRC4 message key = map chr xored
    where
        unicodeMessage = unicodeFromHex message
        messageLength = length unicodeMessage
        keyStream = ksa key
        xored = map (\(messageChar, intFromKeyStream) -> (intFromKeyStream `xor` (ord messageChar))) $ zip unicodeMessage $ prga messageLength keyStream

testDecrypt:: String -> Key -> Bool
testDecrypt message key = decryptRC4 (encryptRC4 message key) key == message



{-********************
    HELPER FUNCTIONS
*********************-}

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