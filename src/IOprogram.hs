module IOProgram(
    encryptIO,
    decryptIO,
    encryptFile,
    exitProgram
) where

import Helpers
import RC4

import Data.List
import Data.Maybe
import Data.Text (splitOn, pack, unpack)
import Data.Char (toUpper)
import Control.Exception


type MainProgram = IO()

encryptIO:: MainProgram -> IO()
encryptIO main = do
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

decryptIO:: MainProgram -> IO()
decryptIO main = do
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

encryptFile:: MainProgram -> IO()
encryptFile main = do
    fileContentsInfo <- putStrLn "Write path to your file."
        >>= return(getLine)
        >>= (\path -> do
                let splitOnDot = splitOn (pack ".") $ pack path
                let splitBySlash = splitOn (pack "/") $ head splitOnDot
                let fileNameExtension = last splitOnDot
                let fileName = last splitBySlash
                let folder = foldr (\x acc -> x++"/"++acc) "" $ map unpack $ init splitBySlash
                return(path ,unpack fileName, folder,unpack fileNameExtension))
        >>= (\(path, fileName, folder, fileNameExtension) -> do
                fileContents <- (try $ readFile path)  :: IO (Either SomeException String)
                case fileContents of 
                    Right cont -> return(Just (cont, fileName, folder,fileNameExtension)) 
                    Left ex    -> return(Nothing))
                    
    case fileContentsInfo of
        Nothing   -> putStrLn "File does not exist."
        Just (cont, fileName, folder, fileNameExtension) -> do
            putStrLn "File loaded succesfully."
            encryptionKey <- putStrLn "Write your encryption key: " >>= return(getLine)
            writeFile (folder ++ "/encrypted_" ++ fileName ++ "." ++ fileNameExtension) $ encryptRC4 cont encryptionKey
    return()

exitProgram:: MainProgram -> IO()
exitProgram main = do
    return()


shouldContinue:: IO(Bool)
shouldContinue = do 
    input <- putStrLn "Do you wish to continue? [y, N]" 
        >>= (\_ -> do
                yesNo <- getLine >>= (\x -> if x == "" then return(Nothing) else return(Just x))
                case yesNo of 
                    Nothing -> return(False)
                    Just x  -> if (toUpper $ head x) == 'Y' then return(True) else return(False))
    return(input)
