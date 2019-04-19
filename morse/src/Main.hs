module Main where

import Control.Monad (forever, when) 
import Data.List (intercalate)
import Data.Traversable (traverse)
import Morse (stringToMorse, morseToChar) 
import System.Environment (getArgs) 
import System.Exit (exitFailure, exitSuccess)
import System.IO (hGetLine, hIsEOF, stdin)


convertToMorse :: IO ()
convertToMorse = convert convertLine
    where
      convertLine line = do 
        let morse = stringToMorse line

        case morse of 
            (Just str) -> putStrLn (intercalate " " str)
            Nothing -> convertError line

convertFromMorse :: IO ()
convertFromMorse = convert convertLine
    where
      convertLine line = do
        let decoded :: Maybe String
            decoded = traverse morseToChar (words line)

        case decoded of
            (Just s) -> putStrLn s
            Nothing -> convertError line

convertError :: String -> IO ()
convertError line = putStrLn ("ERR: " ++ line) >> exitFailure

convert :: (String -> IO ()) -> IO ()
convert convertLine = forever $ do 
    weAreDone <- hIsEOF stdin
    when weAreDone exitSuccess

    (hGetLine stdin) >>= convertLine


main :: IO ()
main = do 
    mode <- getArgs 
    case mode of 
      [arg] ->
        case arg of
          "from" -> convertFromMorse
          "to"   -> convertToMorse
          _      -> argError
      _ -> argError
    
    where argError = putStrLn "Incorrect argument" >> exitFailure
            