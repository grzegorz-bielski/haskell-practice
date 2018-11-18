noLower :: String -> String
noLower str = [ char | char <- str, char `elem` ['A'..'Z']]

-- patterns
lucky :: (Integral a) => a -> String
lucky 7 = "WOHO"
lucky x = "out of luck"

factorial :: (Integral a) => a -> a
factorial n = product [1..n]

factorial' 0 = 1
factorial' n = n * factorial' (n - 1)

addVectors :: (Num a) => (a, a) -> (a, a) -> (a, a)
addVectors (x1, y1) (x2, y2) = (x1 + x2, y1 + y2)

first :: (a, b, c) -> a
first (x, _, _) = x

second :: (a, b, c) -> b
second (_, b, _) = b

third :: (a, b, c) -> c
third (_,_,c) = c

-- xs = [(1,3), (4,3), (4,5)]
reduceTuples :: (Num a) => [(a, a)] -> [a]
reduceTuples xs = [a+b | (a, b) <- xs]

tell :: (Show a) => [a] -> String  
tell [] = "The list is empty"  
tell (x:[]) = "The list has one element: " ++ show x  
tell (x:y:[]) = "The list has two elements: " ++ show x ++ " and " ++ show y  
tell (x:y:_) = "This list is long. The first two elements are: " ++ show x ++ " and " ++ show y  

length' xs = sum [1 | _ <- xs]

length'' :: (Num b) => [a] -> b
length'' [] = 0
length'' (_:xs) = 1 + length'' xs

capital :: String -> String
capital "" = "Empty str"
capital all@(x:xs) = "First letter of" ++ all ++ "is" ++ [x]

head' :: [a] -> a
head' [] = error "empty list"
head' (x:_) = x

-- case
head'' :: [a] -> a
head'' xs = case xs of [] -> error "No head for empty lists!"


-- guards
max' :: (Ord a) => a -> a -> a
max' a b
    | a > b = a
    | otherwise = b

compare' :: (Ord a) => a -> a -> Ordering
a `compare'` b
    | a > b = GT
    | a == b = EQ
    | otherwise = LT

bmiCalc :: (Fractional a) => a -> a -> a
bmiCalc weight height = weight / height ^ 2

-- .. where

bmiTell :: (RealFloat a) => a -> a -> String
bmiTell weight height
    | bmi <= skinny = "underweight"
    | bmi <= normal = "normal"
    | bmi <= fat = "overweight"
    | otherwise = "obese"
    where bmi = bmiCalc weight height
          (skinny, normal, fat) = (18.5, 25.0, 30.0)

initials :: String -> String -> String
initials firstName lastName = [f] ++ ". " ++ [l] ++ "."
    where (f:_) = firstName
          (l:_) = lastName

calcBmis :: (RealFloat a) => [(a, a)] -> [a]
calcBmis xs = [bmi weight height | (weight, height) <- xs]
    where bmi = bmiCalc

fizzBuzzer :: Int -> String
fizzBuzzer x
    | null str = show x
    | otherwise = str
    where str = if x `mod` 3 == 0 then "Fizz" else "" ++ if x `mod` 5 == 0 then "Buzz" else ""

fizzBuzz n = map (fizzBuzzer) [1..n]

-- let .. in

cylinder :: (RealFloat a) => a -> a -> a
cylinder r h =
    let sideArea = 2 * pi * r * h
        topArea = pi * r ^ 2
    in sideArea + 2 * topArea

kek = (let a = 100; b = 200; c = 300 in a * b * c, let foo="Hey "; bar = "there!" in foo ++ bar)

calcBmis' :: (RealFloat a) => [(a, a)] -> [a]
calcBmis' xs = [bmi | (w, h) <- xs, let bmi = w / h ^ 2]

-- rec
fib 0 = 0
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)

maximum' :: (Ord a) => [a] -> a
maximum' [] = error "maximum of empty list"
maximum' [x] = x
maximum' (x:xs)
    | x > maxtail = x
    | otherwise = maxtail
    where maxtail = maximum' xs
