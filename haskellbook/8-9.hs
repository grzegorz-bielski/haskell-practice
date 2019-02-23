module FromChapter8 where

import Data.List (intersperse)

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = Just validResult
    where validResult = if isNegative then (negate $ fst result, snd result) else result
          isNegative = num < 0 || denom < 0
          result = go (abs num) (abs denom) 0
          go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- 8.6
cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"
--
sumRec :: (Eq a, Num a) => a -> a
sumRec n = go 0 n
    where go curr count
            | count == 0 = curr
            | otherwise = go (curr + count) (count - 1)

recMult :: (Integral a) => a -> a -> a
recMult x y = if isNegative then negate result else result
    where result = go 0 0
          isNegative = x < 0 || y < 0
          xAbs = abs x
          yAbs = abs y
          go sum curr
            | curr == yAbs = sum
            | otherwise = go (sum + xAbs) (curr + 1)

--

mc91 :: (Num a, Ord a) => a -> a
mc91 n = if n > 100 then n - 10 else mc91 $ mc91 $ n + 11

--

digitToWord :: Int -> String 
digitToWord n = case n of 0 -> "zero"
                          1 -> "one"
                          2 -> "two"
                          3 -> "three"
                          4 -> "four"
                          5 -> "five"
                          6 -> "six"
                          7 -> "seven"
                          8 -> "eight"
                          9 -> "nine"

digits :: Int -> [Int]
digits 0 = []
digits x = x `mod` 10 : digits (x `div` 10)

wordNumber :: Int -> String 
wordNumber n = parse words
    where words = fmap digitToWord (digits n)
          parse = concat . reverse . intersperse "-"

-- 9.5
eftBool :: Bool -> Bool -> [Bool]
eftBool False True = [False, True]
eftBool a _ = [a]

eft :: (Enum a, Ord a) => a -> a -> [a]
eft a b = reverse $ go a b []
    where go a b acc = if a <= b then go (succ a) b (a : acc) else acc

eftOrd :: Ordering -> Ordering -> [Ordering]
eftOrd = eft

eftInt :: Int -> Int -> [Int]
eftInt = eft

eftChar :: Char -> Char -> [Char]
eftChar = eft

-- 9.6
myWords :: String -> [String]
myWords str = reverse $ go str []
    where go str acc =
            let check = (/=' ')
                strParsed = dropWhile (==' ') str
                word = takeWhile check strParsed
                rest = dropWhile check strParsed
            in if strParsed == "" then acc else go rest (word : acc)

myLines :: String -> [String]
myLines str = reverse $ go str []
        where go str acc = 
                let line = takeWhile (/='\n') str
                    rest = (dropWhile (=='\n') . dropWhile (/='\n')) str
                    soFar = (line : acc)
                in if rest == "" then soFar else go rest soFar

firstSen = "Tyger Tyger, burning bright\n" 
secondSen = "In the forests of the night\n" 
thirdSen = "What immortal hand or eye\n" 
fourthSen = "Could frame thy fearful\
              \ symmetry?"
sentences = firstSen ++ secondSen ++ thirdSen ++ fourthSen

shouldEqual =
    [ "Tyger Tyger, burning bright"
    , "In the forests of the night"
    , "What immortal hand or eye"
    , "Could frame thy fearful symmetry?" ]


main :: IO () 
main =
    print $
    "Are they equal? "
    ++ show (myLines sentences == shouldEqual)

lexer :: Char -> String -> [String]
lexer char str = reverse $ go str []
    where go str acc = 
            let token = takeWhile (/=char) str
                rest = (dropWhile (==char) . dropWhile (/=char)) str
                soFar = (token : acc)
            in if rest == "" then soFar else go rest soFar

-- 9.7
mySqr = [x^2 | x <- [1..5]]
myCube = [y^3 | y <- [1..5]]
tpl = [(x, y) | x <- mySqr, y <- myCube]
tpl' = [(x, y) | x <- mySqr, y <- myCube, x < 50 && y < 50]