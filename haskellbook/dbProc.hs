module DBProc where

import Data.Time

data DBItem = DBString String
            | DBNumber Integer
            | DBDate UTCTime
            deriving (Eq, Ord, Show)

theDB :: [DBItem]
theDB = 
    [ DBDate $ UTCTime (fromGregorian 1911 5 1) (secondsToDiffTime 34123)
    , DBNumber 9001
    , DBString "Hello, world!"
    , DBDate $ UTCTime (fromGregorian 1921 5 1) (secondsToDiffTime 34123)
    ]

filterDBDate :: [DBItem] -> [UTCTime]
filterDBDate = foldr takeTime []
    where takeTime = \curr acc -> case curr of 
            DBDate time -> time : acc
            otherwise -> acc

mostRecent :: [DBItem] -> UTCTime
mostRecent = foldr getOnlyTime zeroDate
    where getOnlyTime = \curr z -> case curr of 
            DBDate time -> if time > z then time else z
            otherwise -> z
          zeroDate = UTCTime (fromGregorian 0 0 0) (secondsToDiffTime 0)

sumDB :: [DBItem] -> Integer
sumDB = foldr sumIt 0
    where sumIt = (\curr acc -> case curr of 
            DBNumber num -> acc + num
            otherwise -> acc)

avgDB :: [DBItem] -> Integer
avgDB xs = sumDB xs `div` toInteger (length xs)

avgDB' :: [DBItem] -> Double
avgDB' xs = fromIntegral $ avgDB xs

