-- 5.8

funcH :: [a] -> a
funcH (x:_) = x

funcC :: (Ord a) => a -> a -> Bool 
funcC x y = if (x > y) then True else False

funcS :: (a, b) -> b
funcS (x, y) = y

i :: a -> a
i x = x

c :: a -> b -> a
c x _ = x

c' :: a -> b -> b 
c' _ x = x

r :: [a] -> [a]
r ls = tail ls

co :: (b -> c) -> (a -> b) -> a -> c
co f1 f2 = f1 . f2

a :: (a -> c) -> a -> a
a _ x = x

a' :: (a -> b) -> a -> b
a' f = f

-- 
ff :: Int -> String
ff = undefined

gg :: String -> Char
gg = undefined

hh :: Int -> Char
hh = gg . ff

--

data A 
data B 
data C

q :: A -> B
q = undefined

w :: B -> C
w = undefined

e :: A -> C
e = w . q

--
data X 
data Y 
data Z

xz :: X -> Z 
xz = undefined

yz :: Y -> Z 
yz = undefined

xform :: (X, Y) -> (Z, Z) 
xform (x, y) = (xz x, yz y)

--

munge :: (x -> y) -> (y -> (w, z)) -> x -> w
munge f g x = fst $ h x
    where h = g . f

--
data Trivial = Trivial'

instance Eq Trivial where
    Trivial' == Trivial' = True

-- 6.5
data TisAnInteger = TisAn Integer deriving (Show)

instance Eq TisAnInteger where
    (==) (TisAn a) (TisAn a') = a == a' 

data TwoIntegers = Two Integer Integer

instance Eq TwoIntegers where
    (==) (Two a b) (Two a' b') = a == a' && b == b'

data StringOrInt = TisAnInt Int | TisAString String

instance Eq StringOrInt where
    (==) (TisAnInt a) (TisAnInt a') = a == a'
    (==) (TisAString a) (TisAString a') = a == a'
    (==) _ _ = False
    
data Pair a = Pair a a

instance Eq a => Eq (Pair a) where
    (==) (Pair a b) (Pair a' b') = a == a'

data Tuple a b = Tuple a b

instance (Eq a, Eq b) => Eq (Tuple a b) where
    (==) (Tuple a b) (Tuple a' b') = a == a' && b == b'

data Which a = ThisOne a | ThatOne a

instance (Eq a) => Eq (Which a) where
    (==) (ThisOne a) (ThisOne a') = a == a'
    (==) (ThatOne a) (ThatOne a') = a == a'
    (==) _ _ = False

data EitherOr a b = Hello a | Goodbye b

instance (Eq a, Eq b) => Eq (EitherOr a b) where
    (==) (Hello a) (Hello a') = a == a'
    (==) (Goodbye b) (Goodbye b') = b == b'
    (==) _ _ = False

-- 6.14
data Person = Person Bool deriving (Show)

printPerson :: Person -> IO ()
printPerson person = putStrLn (show person)
--
data Mood = Blah | Woot deriving (Show, Eq, Ord)

settleDown x = if x == Woot then Blah else x
---

type Subject = String
type Verb = String
type Object = String

data Sentence =
    Sentence Subject Verb Object 
    deriving (Eq, Show)

s1 = Sentence "dogs" "drool"
s2 = Sentence "Julie" "loves" "dogs"
--
data Rocks =
    Rocks String deriving (Eq, Show)
data Yeah =
    Yeah Bool deriving (Eq, Show)
data Papu =
    Papu Rocks Yeah deriving (Eq, Show)
--
chk :: Eq b => (a -> b) -> a -> b -> Bool
chk f a b = f a == b

arith :: Num b => (a -> b) -> Integer -> a -> b
arith f i a = f a + fromInteger i

-- 7
tensDigit :: Integral a => a -> a 
tensDigit x = d
    where xLast = x `div` 10
          d = xLast `mod` 10

tensDigit' :: Integral a => a -> a
tensDigit' x = res
    where (xLast, d) = divMod x 10
          res = if xLast <= 0 then 0 else d

foldBool :: a -> a -> Bool -> a
foldBool x y t = case t of False -> x
                           True -> y

foldBool' :: a -> a -> Bool -> a
foldBool' x y t
    | t == True = y
    | t == False = x

g :: (a -> b) -> (a, c) -> (b, c)
g f (a, c) = (f a,c)

roundTrip :: (Show a, Read a) => a -> a
roundTrip a = read (show a)

roundTrip' :: (Show a, Read a) => a -> a
roundTrip' = read . show