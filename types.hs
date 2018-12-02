module Shapes
( Vector(..)
, Shape
, surface
, move
, baseCircle
, baseRect
) where

import qualified Data.Map as Map

data Vector = Vector Float Float deriving (Show)
data Shape = Circle Vector Float | Rectangle Vector Vector deriving (Show)

surface :: Shape -> Float
surface (Circle _ r) = pi * r ^ 2
surface (Rectangle (Vector x1 y1) (Vector x2 y2)) = (abs $ x2 - x1) * (abs $ y2 - y1)

move :: Shape -> Float -> Float -> Shape
move (Circle (Vector x y) r) a b = Circle (Vector (x+a) (y+b)) r  

baseCircle :: Float -> Shape
baseCircle r = Circle (Vector 0 0) r

baseRect :: Float -> Float -> Shape
baseRect width height = Rectangle (Vector 0 0) (Vector width height)

circleSurface = surface $ Circle (Vector 10 20) 10

data Person = Person { 
    firstName :: String, 
    lastName :: String, 
    age :: Int
} deriving (Read, Show, Eq)

someone = Person { firstName="kek", lastName="kek1", age=22}
saved = read "Person {firstName =\"Michael\", lastName =\"Diamond\", age = 43}" :: Person 

-- maybe and either
data Maybe' a = Nothing' | Just' a
data Either' a b = Left' a | Right' b deriving (Eq, Ord, Read, Show)

data Car a b c = Car {
    company :: a,
    model :: b,
    year :: c
} deriving (Show)

tellCar :: (Show a) => Car String String a -> String
tellCar (Car { company = c, model = m, year = y}) = "This " ++ c ++ " " ++ m ++ "was made in " ++ show y

data Vector' a = Vector' a a a deriving (Show)

vplus :: (Num t) => Vector' t -> Vector' t -> Vector' t
(Vector' i j k) `vplus` (Vector' l m n) = Vector' (i+l) (j+m) (k+n)

data Day = Monday | Tuesday | Wednesday | Thursday | Friday | Saturday | Sunday   
           deriving (Eq, Ord, Show, Read, Bounded, Enum)

type PhoneNumber = String  
type Name = String  
type PhoneBook = [(Name,PhoneNumber)]

inPhoneBook :: Name -> PhoneNumber -> PhoneBook -> Bool  
inPhoneBook name pnumber pbook = (name,pnumber) `elem` pbook

type AssocList k v = [(k,v)]  

--- fef

data LockerState = Taken | Free deriving (Show, Eq)

type Code = String

type LockerMap = Map.Map Int (LockerState, Code)

lockerLookup :: Int -> LockerMap -> Either String Code
lockerLookup lockerNumber map =
    case Map.lookup lockerNumber map of
        Nothing -> Left $ "Locker number " ++ show lockerNumber ++ "doesn't exist!"
        Just (state, code) -> if state /= Taken 
                                then Right code 
                                else Left $ "Locker " ++ show lockerNumber ++ "is already taken!"

lockers :: LockerMap  
lockers = Map.fromList   
    [(100,(Taken,"ZD39I"))  
    ,(101,(Free,"JAH3I"))  
    ,(103,(Free,"IQSA9"))  
    ,(105,(Free,"QOTSA"))  
    ,(109,(Taken,"893JJ"))  
    ,(110,(Taken,"99292"))  
    ]
   
-- custom list
infixr 5 :-:    
data List' a = Empty' | a :-: (List' a) deriving (Show, Read, Eq, Ord)

infixr 5 .++
(.++) :: List' a -> List' a -> List' a
Empty' .++ ys = ys
(x :-: xs) .++ ys = x :-: (xs .++ ys)

aList = 3 :-: 4 :-: Empty'
bList = 2 :-: 1 :-: 5 :-: Empty'
abList = aList .++ bList

-- binary search tree
data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

singleton :: a -> Tree a
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x
treeInsert x (Node a left right)
            | x == a = Node x left right
            | x < a = Node a (treeInsert x left) right
            | x > a = Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
            | x == a = True
            | x < a = treeElem x left
            | x > a = treeElem x right

treeFromList :: (Ord a) => [a] -> Tree a
treeFromList = foldr treeInsert EmptyTree

numsTree = treeFromList [1, 7, 3, 9, 10]

-- class Eq a where  
--     (==) :: a -> a -> Bool  
--     (/=) :: a -> a -> Bool  
--     x == y = not (x /= y)  
--     x /= y = not (x == y)

-- class (Eq a) => Num a where ... (Num is a subclass of Eq)

data TrafficLight = Red | Yellow | Green

instance Eq TrafficLight where
    Red == Red = True
    Green == Green = True
    Yellow == Yellow = True
    _ == _ = False

instance Show TrafficLight where
    show Red = "Red light"
    show Yellow = "Yellow light"
    show Green = "Green light"

instance (Eq m) => Eq (Maybe' m) where  
    Just' x == Just' y = x == y  
    Nothing' == Nothing' = True  
    _ == _ = False

class YesNo a where  
    yesno :: a -> Bool

instance YesNo Int where
    yesno 0 = False
    yesno _ = True

instance YesNo [a] where
    yesno [] = False
    yesno _ = True

instance YesNo Bool where
    yesno = id

instance YesNo (Maybe a) where
    yesno (Just _) = True
    yesno Nothing = False

instance YesNo (Tree a) where  
    yesno EmptyTree = False  
    yesno _ = True

instance YesNo TrafficLight where  
    yesno Red = False  
    yesno _ = True
    
ynIf :: (YesNo a) => a -> b -> b -> b
ynIf ynVal yResult nResult = if yesno ynVal then yResult else nResult

-- functors
-- class Functor f where  
--     fmap :: (a -> b) -> f a -> f b

instance Functor Maybe' where  
    fmap f (Just' x) = Just' (f x)  
    fmap f Nothing' = Nothing'
    
instance Functor Tree where
    fmap f EmptyTree = EmptyTree
    fmap f (Node x leftsub rightsub) = Node (f x) (fmap f leftsub) (fmap f rightsub)

instance Functor (Either' a) where
    fmap f (Right' x) = Right' (f x)
    fmap f (Left' x) = Left' x

-- instance Functor (Map.Map k) where
--     fmap f empty = Map.empty
--     fmap = Map.map