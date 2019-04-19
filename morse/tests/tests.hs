module Main where

import qualified Data.Map as M 
import Morse
import Test.QuickCheck
import Data.List (sort)


allowedChars :: [Char]
allowedChars = M.keys letterToMorse

allowedMorse :: [Morse]
allowedMorse = M.elems letterToMorse

charGen :: Gen Char
charGen = elements allowedChars

morseGen :: Gen Morse
morseGen = elements allowedMorse

prop_thereAndBackAgain :: Property
prop_thereAndBackAgain =
    forAll charGen isTheSame
    where isTheSame c = (charToMorse c >>= morseToChar) == Just c


main = quickCheck prop_thereAndBackAgain

-- arbitrary

data Trivial = Trivial deriving (Eq, Show)

trivialGen :: Gen Trivial 
trivialGen = pure Trivial

instance Arbitrary Trivial where
    arbitrary = trivialGen

trivialSample :: IO ()
trivialSample = sample trivialGen

---

data Identity a = Identity a deriving (Eq, Show)

identityGen :: Arbitrary a => Gen (Identity a)
identityGen = arbitrary >>= (\a -> pure (Identity a))

instance Arbitrary a => Arbitrary (Identity a) where
    arbitrary = identityGen

identityIntSample = sample (identityGen :: Gen (Identity Int))


--- 

data Sum a b = First a | Second b deriving (Eq, Show)

sumGenEqual :: (Arbitrary a, Arbitrary b) => Gen (Sum a b)
sumGenEqual = do 
    a <- arbitrary
    b <- arbitrary
    oneof [pure $ First a, pure $ Second b]

sumCharIntSample = sample (sumGenEqual :: Gen (Sum Char Int))

---

-- prop_thereAndBackAgain :: Property
-- prop_thereAndBackAgain =
--     forAll charGen isTheSame
--     where isTheSame c = (charToMorse c >>= morseToChar) == Just c

half x = x / 2
halfIdentity = (*2) . half

prop_hasIdentity :: Double -> Bool
prop_hasIdentity x = half x == (halfIdentity x) / 2

checkFunck = quickCheck prop_hasIdentity

--
listOrdered :: (Ord a) => [a] -> Bool 
listOrdered xs =
    snd $ foldr go (Nothing, True) xs 
    where go _ status@(_, False) = status 
          go y (Nothing, t) = (Just y, t)
          go y (Just x, t) = (Just y, x >= y)


checkList = quickCheck (listOrdered :: [Char] -> Bool)

--
plusAssociative x y z = x + (y + z) == (x + y) + z
plusCommutative x y = x + y == y + x

checkAssociativity = quickCheck (plusAssociative :: Int -> Int -> Int -> Bool)
checkCommutativity = quickCheck (plusCommutative :: Int -> Int -> Bool)

--
--  division truncated
quotRemLaw x y = (x `quot` y)*y + (x `rem` y) == x -- toward zero
divModLaw x y = (x `div`  y)*y + (x `mod` y) == x -- toward negative infinity





    