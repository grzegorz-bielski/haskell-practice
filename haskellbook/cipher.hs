module Cipher 
(caesar) where

import Data.Char

caesar :: Int -> String -> String
caesar shft "" = ""
caesar shft (x:xs) = chr validShift : caesar shft xs
    where shifted = ord x + shft
          rightShift = shft >= 0
          getValidShift = if isUpper x then getShift 65 90 else getShift 97 122
          validShift = getValidShift shifted rightShift

getShift :: Int -> Int -> Int -> Bool -> Int
getShift start end shifted rightShift = validShift
    where validShift
            | rightShift = if shifted > end then start + shifted - 1 - end else shifted
            | otherwise = if shifted < start then end + shifted + 1 - start else shifted
