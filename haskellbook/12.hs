module FromChapter12 where

notThe :: String -> Maybe String
notThe str = if isThe str then Nothing else Just str

replaceThe :: String -> String
replaceThe = unwords . fmap (replaceWord "a" . notThe) . words
    where replaceWord a Nothing = a
          replaceWord _ (Just word) = word

isVowel :: Char -> Bool
isVowel c = c `elem` "aeiou"

isThe :: String -> Bool
isThe c = c == "the"

firstHasVowel :: [String] -> Bool
firstHasVowel [] = False
firstHasVowel (x:_) 
  | x /= "" && isVowel (head x) = True
  | otherwise = False

incrementCounter :: [String] -> Integer
incrementCounter [] = 0
incrementCounter (x:xs)
    | isThe x && firstHasVowel xs = 1
    | otherwise = 0

countTheBeforeVowel :: String -> Integer
countTheBeforeVowel "" = 0 
countTheBeforeVowel str = count $ incrementCounter ws
    where ws = words str 
          count n = n + countTheBeforeVowel (unwords $ drop 1 ws)

countChar :: (Char -> Bool) -> String -> Integer -> Integer 
countChar f "" acc = acc
countChar f (x:xs) acc = countChar f xs $ if f x then (acc + 1) else acc

countVowels :: String -> Integer
countVowels str = countChar isVowel str 0

--

countConsonants :: String -> Integer
countConsonants str = countChar (not . isVowel) str 0

newtype Word' = Word' String deriving (Eq, Show)

mkWord :: String -> Maybe Word'
mkWord str = if countVowels str > countConsonants str 
                then Nothing 
                else Just (Word' str)

--
data Natural = Zero | Succ Natural deriving (Eq, Show)

integerToNatural :: Integer -> Maybe Natural
integerToNatural n
    | n > 0 =  Just $ Succ (unWrap $ integerToNatural $ n - 1)
    | n == 0 = Just Zero
    | otherwise = Nothing
    where unWrap (Just a) = a

naturalToInteger :: Natural -> Integer
naturalToInteger Zero = 0
naturalToInteger (Succ nat) = 1 + naturalToInteger nat

--

-- 1.

isJust :: Maybe a -> Bool
isJust (Just _) = True
isJust Nothing = False 

isNothing :: Maybe a -> Bool
isNothing = not . isJust

-- 2.

maybeCat :: b -> (a -> b) -> Maybe a -> b
maybeCat acc f Nothing = acc
maybeCat acc f (Just a) = f a 

-- 3.

fromMaybe :: a -> Maybe a -> a
fromMaybe a (Just b) = b
fromMaybe a Nothing = a

-- 4.

listToMaybe :: [a] -> Maybe a
listToMaybe [] = Nothing
listToMaybe a = Just (head a)

maybeToList :: Maybe a -> [a]
maybeToList Nothing = []
maybeToList (Just a) = [a]

-- 5.

catMaybes :: [Maybe a] -> [a]
catMaybes = foldr onlyJusts []
    where onlyJusts (Just a) acc = a : acc
          onlyJusts Nothing acc = acc

flipMaybe :: [Maybe a] -> Maybe [a]
flipMaybe xs
    | all isJust xs = Nothing
    | otherwise = Just $ catMaybes xs

----

-- 1.

lefts' :: [Either a b] -> [a]
lefts' = foldr takeLeft []
    where takeLeft (Left a) acc = a : acc
          takeLeft (Right _) acc = acc

-- 2.

rights' :: [Either a b] -> [b]
rights' = foldr takeRight []
    where takeRight (Right b) acc = b : acc
          takeRight (Left a) acc = acc

-- 3.

partitionEither' :: [Either a b] -> ([a], [b])
partitionEither' = foldr takeFromSide ([],[])
    where takeFromSide (Left a) (left, right) = (a : left, right)
          takeFromSide (Right b) (left, right) = (left, b : right)

-- 4.

eitherMaybe' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe' f (Left a) = Nothing
eitherMaybe' f (Right b) = Just $ f b

-- 5.

either' :: (a -> c) -> (b -> c) -> Either a b -> c
either' f _ (Left a) = f a
either' _ g (Right b) = g b
          
-- 6.

eitherMaybe'' :: (b -> c) -> Either a b -> Maybe c
eitherMaybe'' f = either' (const Nothing) (\b -> Just $ f b)

--

-- 1.

myIterate :: (a -> a) -> a -> [a]
myIterate f acc = [acc] <> myIterate f (f acc)

-- 2.

myUnfoldr :: (b -> Maybe (a, b)) -> b -> [a]
myUnfoldr f b = case f b of
        Nothing -> []
        Just (a, b') -> a : myUnfoldr f b'

-- 3.

betterIterate :: (a -> a) -> a -> [a]
betterIterate f = myUnfoldr (\b -> Just (b, f b))

-- --

data BinaryTree a = Leaf | Node (BinaryTree a) a (BinaryTree a) deriving (Eq, Ord, Show)

treeUnfold :: (a -> Maybe (a,b,a)) -> a -> BinaryTree b
treeUnfold f a = case f a of
        Nothing -> Leaf
        Just (leftBranch, b, rightBranch) -> Node (treeUnfold f leftBranch) b (treeUnfold f rightBranch ) 

---

treeBuild :: Integer -> BinaryTree Integer
treeBuild n = treeUnfold (\a -> if a < 2^n - 1
                            then Just (2*a+1, a, 2*a+1)
                            else Nothing
                         ) 0

