compareto100 :: (Num a, Ord a) => a -> Ordering
compareto100 = compare 100

divideByTen :: (Floating a ) => a -> a
divideByTen = (/10)

applyTwice :: (a -> a) -> a -> a
applyTwice f x = f (f x)

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys

flip' :: (a -> b -> c) -> b -> a -> c
flip' f x y = f y x

-- [5, 4, 3, 2, 1]
arr = zipWith' (flip' div) [2, 2..] [10,8,6,4,2]

map' :: (a -> b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map f xs

filter' :: (a -> Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs

largetDivisible :: (Integral a) => a
largetDivisible = head (filter p [100000, 99999..])
    where p x = x `mod` 3829 == 0

takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' _ [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

sumOfAllOddSquaresBelow:: Int -> Int  
sumOfAllOddSquaresBelow n = sum (takeWhile' (<n) (filter odd (map (^2) [1..])))

-- collatz sequences
chain :: (Integral a) => a -> [a]
chain 0 = error "Chain can't start at 0"
chain 1 = [1]
chain n
    | even n = n:chain (n `div` 2)
    | odd n = n:chain (n*3 + 1)

numLongChains :: Int
numLongChains = length (filter isLong (map chain [1..100]))
    where isLong xs = length xs > 15

twenty = (map (*) [0..] !! 4) 5

-- lambdas

numLongChains' :: Int
numLongChains' = length (filter (\xs -> length xs > 15) (map chain [1..100]))

-- foldl -> reduceLeft
sum' :: (Num a) => [a] -> a
sum' xs = foldl (+) 0 xs

-- fold1 - take [0] as an acc
-- fold1' - not lazy acc (without thunks)

elem' :: (Eq a) => a -> [a] -> Bool
elem' y ys = foldl (\acc x -> if x == y then True else acc) False ys

map'' :: (a -> b) -> [a] -> [b]
map'' f xs = foldr (\x acc -> f x : acc) [] xs

-- scanl (+) 0 [3,5,2,1]
sqrtSums :: Int
sqrtSums = length (takeWhile (<1000) (scanl1 (+) (map sqrt [1..]))) + 1

-- function application
-- ($) :: (a -> b) -> a -> b
-- f $ x = f x
firstTenSqrtsSum = sum (map sqrt [1..10])
firstTenSqrtsSum' = sum $ map sqrt [1..10]

-- function composition
-- (.) :: (b -> c) -> (a -> b) -> a -> c
-- f . g = \x -> f (g x)
negative = map $ negate . abs

-- point free style (without explicit parameter)
fn = ceiling . negate . tan . cos . max 50