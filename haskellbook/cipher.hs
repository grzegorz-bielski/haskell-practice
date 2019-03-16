module Cipher 
(caesar, vigenere) where

import Data.Char
import Data.List
import Debug.Trace

caesar :: Int -> String -> String
caesar shft "" = ""
caesar shft (x:xs) = chr validShift : caesar shft xs
    where shifted = ord x + shft
          rightShift = shft >= 0
          validShift = getValidShift x shifted rightShift

getValidShift :: Char -> Int -> Bool -> Int
getValidShift c = if isUpper c then getShift 65 90 else getShift 97 122

getShift :: Int -> Int -> Int -> Bool -> Int
getShift start end shifted rightShift = validShift
    where validShift
            | rightShift = 
                if shifted > end then start + shifted - 1 - end else shifted
            | otherwise = 
                if shifted < start then end + shifted + 1 - start else shifted

vigenere :: String -> String -> Maybe String
vigenere key = foldl' (\acc text -> 
    concatElem <$> acc <*> vigenereCipher key text) (Just "") . words
    where concatElem a b = if a == "" then a <> b else a <> " " <> b

-- matrix based approach, a bit long...
vigenereCipher :: String -> String -> Maybe String
vigenereCipher key text = sequence ciphered
    where 
          keystream :: String
          keystream = take (length text) $ cycle key

          ciphered :: [Maybe Char]
          ciphered = fmap getCipherChar $ zip text keystream

          getCipherChar :: (Char, Char) -> Maybe Char
          getCipherChar (c,k) = sequence [elemIndex (toLower c) abc, elemIndex (toLower k) abc] 
             >>= (\[cShift, kShift] ->  Just $ (drop kShift (cycle abc)) !! cShift)
             where abc = ['a'..'z']