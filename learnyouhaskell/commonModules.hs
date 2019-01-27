import Data.List
import Data.Char

import qualified Data.Map as Map
import qualified Data.Set as Set

import qualified Geometry.Sphere as Sphere  
import qualified Geometry.Cuboid as Cuboid  
import qualified Geometry.Cube as Cube  
-- Data.List
-- 
-- solve: 3x2 + 5x + 9, 10x3 + 9 and 8x3 + 5x2 + x - 1
x = map sum $ transpose [[0,3,5,9], [10,0,0,9],[8,5,1,-1]]

withSpace = intercalate " " ["hey", "there", "guys"]
withDot = intersperse '.' "DOT"

concatenated = concat ["foo","bar","car"]  

anyEqualsFour = any (==4) [2,3,4,5,6,1]
allEqualsFour = all (==4) [2,3,4,5,6,1]

firstTenIterated = take 10 $ iterate (*2) 1

splitAtThree :: [a] -> ([a], [a])
splitAtThree arr = splitAt 3 arr

takeWhileNotSpace :: String -> String
takeWhileNotSpace str = takeWhile (/=' ') str
-- + dropWhile

sentenceSplitter :: String -> String
sentenceSplitter str = "First word: " ++ fw ++ ", the rest: " ++ rest
    where (fw, rest) = span (/=' ') str

bigAndSmall :: String -> (String, String)
bigAndSmall = partition (`elem` ['A'..'Z'])

break' :: (a -> Bool) -> [a] -> ([a], [a])
break' p = span $ not . p 

howManyTimes :: (Ord a) => [a] -> [(a, Int)]
howManyTimes = map (\l@(x:xs) -> (x, length l)) . group . sort

isInfixOf' :: (Eq a) => [a] -> [a] -> Bool
isInfixOf' thing place = foldl (\acc x -> if take nlen x == thing then True else acc) False (tails place)
    where nlen = length thing

indicesOfNothing :: String -> [Int]
indicesOfNothing arr = ' ' `elemIndices` arr
-- + elemIndex, find and findIndex

-- lines, unlines, words, unwords
-- nub -> uniqe

withoutH :: String -> String
withoutH = delete 'h'

-- set difference
notInTens :: (Eq a, Num a, Enum a) => [a] -> [a]
notInTens arr = [1..10] \\ arr
-- set union: `union`
-- set intersection: `intersect`

-- insertAndPreserveSorting
insertAndPreserveSorting = insert 4 

belowAndAboveZero :: (Num a, Ord a) => [a] -> [[a]]
belowAndAboveZero = groupBy (\x y -> (x > 0) == (y > 0))

on' :: (b -> b -> c) -> (a -> b) -> a -> a -> c
f `on'` g = \x y -> f (g x) (g y)

belowAndAboveZero' :: (Num a, Ord a) => [a] -> [[a]]
belowAndAboveZero' = groupBy ((==) `on'` (> 0))

-- Data.Char
-- --

isAllAlphaNum :: String -> Bool
isAllAlphaNum = all isAlphaNum

words' :: String -> [String]
words' str = filter (not . any isSpace) . groupBy ((==) `on'` isSpace) $ str

isSpace' :: Char -> Bool
isSpace' char = generalCategory char == Space
-- digitToInt <-> intToDigit
-- chr: int -> char

encode :: Int -> String -> String
encode shift = map $ chr . (+ shift) . ord

decode :: Int -> String -> String
decode shift = encode $ negate shift

caesarEncode :: String -> String
caesarEncode = encode 32

caesarDecode :: String -> String
caesarDecode = decode 32      

phoneBook =   
    [("betty","555-2938")  
    ,("bonnie","452-2928")  
    ,("patsy","493-2928")  
    ,("lucille","205-2928")  
    ,("wendy","939-8282")  
    ,("penny","853-2492")
    ,("penny","555-2111")  
    ]  

findKey' :: (Eq k) => k -> [(k,v)] -> v
findKey' key xs = snd . head . filter (\(k,v) -> key == k) $ xs

findKey'' :: (Eq k) => k -> [(k,v)] -> Maybe v
findKey'' key [] = Nothing
findKey'' key ((k,v):xs)
    | key == k = Just v
    | otherwise = findKey'' key xs

-- Data.Map
phoneBookMap = Map.fromList phoneBook
emptyMap = Map.empty
phoneBookUpdated = Map.insert ("kek", "34-45") . Map.insert ("NANI", "34-45e") $ phoneBookMap
-- + insertWith

fromList' :: (Ord k) => [(k,v)] -> Map.Map k v
fromList' = foldr (\(k, v) acc -> Map.insert k v acc) Map.empty

-- Map.null - checks if empty, Map.size, Map.member
oneElement = Map.singleton 3 9

-- Map.keys
keys' :: Map.Map k v -> [k]
keys' = map fst . Map.toList 

phoneBookToMap :: (Ord k) => [(k, String)] -> Map.Map k String
phoneBookToMap = Map.fromListWith (\nr1 nr2 -> nr1 ++ ", " ++ nr2)

-- Data.Set
text1 = "lorem ipsum doe nah"  
text2 = "The old man left his garbage can out and now his trash is all over my lawn!"

-- intersection, difference, union, etc.
uniqueChars :: Ord a => ([a], [a]) -> Set.Set a
uniqueChars (one, two) = Set.intersection (Set.fromList one) (Set.fromList two)

thingies = [1,2,3,4,5]
isSubset = Set.fromList thingies `Set.isSubsetOf` Set.fromList thingies
isProperSubset =  Set.fromList thingies `Set.isProperSubsetOf` Set.fromList thingies

-- preserves the ordering
setNub :: Ord a => [a] -> [a]
setNub xs = Set.toList $ Set.fromList xs