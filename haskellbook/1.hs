module FirstChapter where

import Data.Char

-- 2.4
-- -//-

-- 2.5. eval

-- 1. 
half x = x / 2
square x = x * x

halfSquared = half . square

-- 2.
circleField :: (Floating a) => a -> a
circleField n = pi * (n * n)

-- 3.
-- -//-

-- 2.6

-- 1. yes 
-- 2. no
-- 3. yes

-- 2.7

-- 1.
area x = 3.14 * (x * x)
-- 2.
double z = z * 2
-- 3
x = 7
y = 10
f = x + y

-- 2.8
integral division = (quot x y) * y + (rem x y) == x
modular division = (div x y) * y + (mod x y) == x

-- 2.10
res = let x = 5; y = 6 in x * y
res' = z / y + y
    where x = 7
          y = negate x
          z = y * 10

-- 3.2
area' d = pi * (r * r)
    where r = d / 2

-- 3.5
rvrs :: String
rvrs = one ++ two ++ three
    where str = "Curry is awesome"
          one = drop 9 str
          two = take 4 $ drop 5 str
          three = take 5 str

-- 4.3
data Mood = Blah | Woot deriving Show

changeMood :: Mood -> Mood
changeMood Blah = Woot
changeMood _ = Blah

-- 4.9
awesome = ["Papuchon", "curry", ":)"]
also = ["Quake", "The Simons"]
allAwesome = [awesome, also]

isPalindrome :: String -> Bool
isPalindrome str = lowered == reversed
    where lowered = fmap toLower str
          reversed = reverse lowered

abs' :: Integer -> Integer
abs' x = if x < 0 then negate x else x

thisF :: (a, b) -> (c, d) -> ((b, d), (a, c))
thisF (a, b) (c, d) =  ((b, d), (a, c))

xx = (+)
ff :: (Foldable f) => f a -> Int
ff xs = w `xx` 1
    where w = length xs

id' x = x 

