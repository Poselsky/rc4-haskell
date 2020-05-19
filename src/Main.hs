module Main where

import Data.Maybe
import Text.Read (readMaybe)

import IOProgram


main :: IO()
main = do
    let mainFunctions = [encryptIO, decryptIO, encryptFile, exitProgram]
    input <- mapM_ putStrLn ["Please choose an option (1. Default):", "1. Encrypt message", "2. Decrypt message", "3. Encrypt file" ,"4. Exit application"] 
            >>= return(getLine)
            >>= (\x -> return (readMaybe x :: Maybe Int))
            >>= (\x -> case x of    Just x    -> if x <= length mainFunctions && x > 0 then return(Just x) else return(Nothing)
                                    otherwise -> return(Just 1))
    
    case input of   Nothing      -> do 
                                putStrLn "Please, choose correct item from menu."
                                main
                    -- -1 is because of menu selection, in Haskell, indexing starts at 0
                    Just input -> (mainFunctions !! (input - 1)) main
                    
    return()