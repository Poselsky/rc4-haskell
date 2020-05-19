module RC4 (
    encryptRC4,
    decryptRC4,
    testDecrypt
) where

import Helpers

import Data.List
import Data.Char (chr, ord, intToDigit, toUpper)
import Data.Bits (xor)
import Numeric

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
