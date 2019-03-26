{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances #-}

module FromChapter11 where

import Data.Char
import qualified Data.List as L
import Control.Arrow

-- 11.9
class TooMany a where 
    tooMany :: a -> Bool

instance TooMany Int where 
    tooMany n = n > 42

newtype Goats =
    Goats Int deriving (Eq, Show, TooMany)

instance TooMany (Int, String) where
    tooMany (n,_) = n > 42

instance TooMany (Int, Int) where
    tooMany (n, m) = (n + m) > 42

-- 11.13
data OperatingSystem = 
      GnuPlusLinux
    | OpenBSDPlusNevermindJustBSDStill 
    | Mac
    | Windows
    deriving (Eq, Show)

data ProgLang =
    Haskell
    | Agda
    | Idris
    | PureScript deriving (Eq, Show)

data Programmer =
    Programmer { os :: OperatingSystem
               , lang :: ProgLang 
               } 
    deriving (Eq, Show)

allOperatingSystems :: [OperatingSystem] 
allOperatingSystems =
    [ GnuPlusLinux
    , OpenBSDPlusNevermindJustBSDStill 
    , Mac
    , Windows
    ]
 
allLanguages :: [ProgLang] 
allLanguages =
     [Haskell, Agda, Idris, PureScript]

allProgrammers :: [Programmer]
allProgrammers = foldr (
    \os acc -> acc <> (fmap (\lang -> Programmer{ os = os, lang = lang}) allLanguages) 
    ) [] allOperatingSystems

-- 11.7

data BinaryTree a = 
    Leaf
  | Node (BinaryTree a) a (BinaryTree a) 
   deriving (Eq, Ord, Show)

mapTree :: (a -> b)
            -> BinaryTree a
            -> BinaryTree b
mapTree _ Leaf = Leaf
mapTree f (Node left a right) =
    Node (mapTree f left) (f a) (mapTree f right)

testTree' :: BinaryTree Integer
testTree' =
    Node (Node Leaf 3 Leaf) 
          1
         (Node Leaf 4 Leaf)

mapExpected =
    Node (Node Leaf 4 Leaf)
          2
          (Node Leaf 5 Leaf)

mapOkay =
    if mapTree (+1) testTree' == mapExpected 
    then print "yup okay!"
    else error "test failed!"

preorder :: BinaryTree a -> [a]
preorder Leaf = []
preorder (Node left a right) = [a] <> (preorder left) <> (preorder right)

inorder :: BinaryTree a -> [a]
inorder Leaf = [] 
inorder (Node left a right) = (inorder left) <> [a] <> (inorder right)

postorder :: BinaryTree a -> [a]
postorder Leaf = [] 
postorder (Node left a right) = (postorder left) <> (postorder right) <> [a]

testTree :: BinaryTree Integer
testTree =
    Node (Node Leaf 1 Leaf) 
    2
    (Node Leaf 3 Leaf)

testPreorder :: IO () 
testPreorder =
    if preorder testTree == [2, 1, 3] 
    then putStrLn "Preorder fine!" 
    else putStrLn "Bad news bears."

testInorder :: IO () 
testInorder =
    if inorder testTree == [1, 2, 3] 
    then putStrLn "Inorder fine!" 
    else putStrLn "Bad news bears."

testPostorder :: IO () 
testPostorder =
    if postorder testTree == [1, 3, 2] 
    then putStrLn "Postorder fine!"
    else putStrLn "postorder failed check"


treeFoldr :: (a -> b -> b) -> b -> BinaryTree a -> b
treeFoldr _ acc Leaf = acc
treeFoldr f acc (Node left a right) = treeFoldr f (treeFoldr f (f a acc) left) right

isSubseqOf :: Eq a => [a] -> [a] -> Bool
isSubseqOf sub whole = sub == filter (\x -> x `elem` sub) whole

capitalizeWords :: String -> [(String, String)]
capitalizeWords = fmap tuplify . words
    where tuplify str = (str, capitalizeWord str)
    
capitalizeWord :: String -> String
capitalizeWord (w:ws) = (toUpper w) : ws

splitWhen :: (Char -> Bool) -> String -> [String]
splitWhen f s =  case dropWhile f s of
                      "" -> []
                      s' -> w : splitWhen f s''
                            where (w, s'') = break f s'

capitalizeParagraph :: String -> String
capitalizeParagraph = foldr (\x curr -> capitalizeWord x <> "." <> curr) "" . splitWhen (=='.')

---
type Key = Char
type KeyValues = String

data DaPhone = DaPhone [(Key, KeyValues)]

daPhone = DaPhone
    [('1', ""),
     ('2', "abc"),
     ('3', "def"),
     ('4', "ghi"),
     ('5', "jkl"),
     ('6', "mno"),
     ('7', "pqrs"),
     ('8', "tuv"),
     ('9', "wxyz"),
     ('*', "^"),
     ('0', " +_"),
     ('#', ".,")
     ]

convo :: [String] 
convo =
    ["Wanna play 20 questions",
     "Ya",
     "U 1st haha",
     "Lol ok. Have u ever tasted alcohol",
     "Lol ya",
     "Wow ur cool haha. Ur turn",
     "Ok. Do u think I am pretty Lol",
     "Lol ya",
     "Just making sure rofl ur turn"]


type Digit = Char
type Presses = Int

reverseTaps :: DaPhone -> Char -> [(Digit, Presses)] 
reverseTaps (DaPhone phone) ch = if isUpper ch then  ('*', 1) : rest else rest
    where rest = foldr untap [] phone
          char = toLower ch
          untap (digit, chars) acc = if length indices > 0 then (digit, head indices + 1) : acc else acc
            where indices = indicesOf char chars
     
indicesOf :: Eq a => a -> [a] -> [Int]
indicesOf elt list = fmap fst $ filter ((elt==).snd) $ zip [0..] list

cellPhonesDead :: DaPhone -> String -> [(Digit, Presses)] 
cellPhonesDead daPhone str = concat $ fmap (reverseTaps daPhone) str

notDeadYet :: DaPhone -> [String] -> [[(Digit, Presses)]]
notDeadYet daPhone = fmap (cellPhonesDead daPhone)

fingerTaps :: [(Digit, Presses)] -> Presses 
fingerTaps = foldr (\x acc -> acc + snd x) 0

mostPopular :: Ord a => [a] -> a
mostPopular = snd . head . reverse . L.sort . frequency

frequency :: Ord a => [a] -> [(Int,a)]
frequency = fmap (length &&& head) . L.group . L.sort

coolestLtr :: [String] -> Char
coolestLtr = mostPopular . fmap mostPopular

coolestWord :: [String] -> String
coolestWord = mostPopular

---
data Expr = Lit Integer | Add Expr Expr

eval :: Expr -> Integer
eval (Lit value) = value
eval (Add exp exp') = (+) (eval exp) (eval exp')

printExpr :: Expr -> String
printExpr (Lit n) = show n
printExpr (Add exp exp') = (printExpr exp) <> " + " <> (printExpr exp')
