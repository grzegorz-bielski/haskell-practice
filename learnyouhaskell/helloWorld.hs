import Data.Char

doubleMe x = x + x
doubleUs x y = doubleMe x + doubleMe x
doubleSmall x = if x > 100 then x else x * 2
defFunc = "hello world"
-- lists

-- long
nums = [1, 3, 4 , 55, 3]
numss = 1:2:3:[]
numstwo = nums ++ [1]
-- faster
numsthree = 1:nums
acat = 'a':"cat"
thirdLetter list = list !! 3
catHead = head acat
catTail = tail acat
catlast = last acat
everyThingEceptLast = init acat
catLength = length acat
isCatEmpty = null acat
takeFirstThree = take 3 acat
dropFirstThree = drop 3 acat
numProduct = product [2, 3, 2]
isInList = 'a' `elem` acat
-- lexicographical order for compar
isBigger1 = [3,2,1] > [2,1,0]
isBigger2 = [3,2,1] > [2, 10, 100]

-- ranges
twenty = [1..20]
twentyRev = [20, 19..1]
evenTwenty = [2,4..20]
firstTen = take 10 (cycle [1, 2, 3])
firstFive = take 5 (repeat 5)
tens = 3 `replicate` 10

-- comprahensions
evensBiggerThan10 = [x*2 | x <- [1..10], x*2 >= 12]
boomBang xs = [ if x < 10 then "Boom!" else "BANG" | x <- xs, odd x, x /= 15]
multiplyTwo a b =  [ x*y | x <- a, y <- b]
length' xs = sum [1 | _ <- xs]

isPalindrome str = let lowered = map toLower str in reverse lowered == lowered

-- tuples
first = fst (8, 10)
second = snd (8, 10)
zipped = zip [1..] ["kek", "kek", "kek"]

triangles = [(a,b,c) | c <- [1..10], b <- [1..c], a <- [1..b], a^2 + b^2 == c^2, a+b+c == 24]
