{-# LANGUAGE FlexibleInstances #-}

module Main where

import Lib
import Data.Monoid
import Test.QuickCheck
import Test.QuickCheck.Checkers
import Test.QuickCheck.Classes
import GHC.Arr
import Data.List (elemIndex)
import Control.Applicative (liftA3)

-- Functors
--------------------

a = fmap (+1) $ read "[1]" :: [Int]
b = (fmap . fmap) (++ "lol") $ Just ["a", "b"]
c = fmap (*2) (\x -> x - 2)
-- fmap for fun functor is it's composition
c' = (*2) <$> (\x -> x - 2) 
d = ((pure '1' <>) . show) . (\x -> [x, 1..3])
d' = (\x -> "1" <> x) . show . (\x -> [x, 1..3])

e :: IO Integer 
e = let ioi = readIO "1" :: IO Integer 
        changed = read <$> ("123" ++) <$> show <$> ioi
    in (*3) <$> changed

-- functor laws:
-- fmap id = id
-- fmap (p . q) = (fmap p) . (fmap q)

functorIdentity :: (Functor f, Eq (f a)) => f a -> Bool
functorIdentity f = fmap id f == f

functorCompose :: (Eq (f c), Functor f) => (a -> b) -> (b -> c) -> f a -> Bool 
functorCompose f g x = (fmap g (fmap f x)) == (fmap (g . f) x)

newtype Identity a = Identity a deriving (Eq, Ord, Show)

instance Functor Identity where
    fmap f (Identity val) = Identity $ f val

data Pair a = Pair a a 

instance Functor Pair where 
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)
    
data Two a b = Two a b 

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two f g) (Two a b) = Two (f <> a) (g b)

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

data Three' a b = Three' a b b 

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

data Four a b = Four a a a b 

instance Functor (Four a) where
    fmap f (Four a b c d) = Four a b c (f d)

-- cant implement
data Trivial = Trivial 

data Option a = None | Some a deriving (Eq, Show)

instance Functor Option where
    fmap f None = None 
    fmap f (Some a) = Some $ f a

data Sum' a b = First' a | Second b deriving (Eq, Show)

instance Functor (Sum' a) where
    fmap f (First' c) = First' c
    fmap f (Second b) = Second $ f b

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

---
data Company a b c = DeepBlue a c | Something b

instance Functor (Company a b) where
    fmap f (DeepBlue a c) = DeepBlue a (f c)
    fmap _ (Something b) = Something b

data More b a = L a b a | R b a b deriving (Eq, Show)

instance Functor (More x) where
    fmap f (L a b a') = L (f a) b (f a') 
    fmap f (R b a b') = R b (f a) b'

---

data Quant a b =
      Finance
    | Desk a
    | Bloor b

instance Functor (Quant a) where
    fmap f (Bloor b) = Bloor $ f b
    fmap _ (Desk a) = Desk a
    fmap _ (Finance) = Finance

data K a b = K a 

instance Functor (K a) where
    fmap _ (K a) = K a

newtype Flip f a b = Flip (f b a) deriving (Eq, Show)

-- f x y = (x, y)
-- g = f 1 2 
-- g' = Flip f
-- g'' = g'
-- newtype K' a b = K' a

instance Functor (Flip K a) where
    -- fmap f (Flip (K a)) = Flip . K . f $ a
    fmap f (Flip (K a)) = Flip <$> K <$> f $ a

data EvilGoateeConst a b = GoatyConst b 

instance Functor (EvilGoateeConst a) where
    fmap f (GoatyConst b) = GoatyConst $ f b

data LiftItOut f a = LiftItOut (f a)

instance Functor f => Functor (LiftItOut f) where
    fmap f (LiftItOut f') = LiftItOut $ f <$> f'

data Parappa f g a = DaWrappa (f a) (g a)

instance (Functor f, Functor g) => Functor (Parappa f g) where
    fmap f (DaWrappa f' f'') = DaWrappa (f <$> f') (f <$> f'')

data IgnoreOne f g a b = IgnoreSomething (f a) (g b)

instance Functor g => Functor (IgnoreOne f g a) where
    fmap f (IgnoreSomething f' f'') = IgnoreSomething f' $ f <$> f''

data Notorious g o a t = Notorious (g o) (g a) (g t)

instance Functor g => Functor (Notorious g o a) where 
    fmap f (Notorious a b c) = Notorious a b (f <$> c)

----------

data List a = 
      Nil 
    | Cons a (List a) deriving (Eq)

instance Show a => Show (List a) where
    show a = "funny" ++ show a

instance Semigroup (List a) where
    (<>) Nil ys = ys
    (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)

instance Functor List where
    fmap _ Nil = mempty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _ = Nil
    (<*>) (Cons f fs) values = (f <$> values) <> (fs <*> values)

instance Foldable List where
    foldr _ a Nil = a 
    foldr f a (Cons x xs) = f x $ foldr f a xs

concat' :: List (List a) -> List a
concat' = foldr (<>) Nil

instance Monad List where
    return = pure
    (>>=) xs f = concat' $ f <$> xs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = (\a -> Cons a Nil) <$> arbitrary

instance Eq a => EqProp (List a) where
    (=-=) xs ys = (take' 1000 xs) `eq` (take' 1000 ys)

take' :: Int -> List a -> List a
take' 0 _ = Nil
take' _ Nil = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)
--------

data GoatLord a =
      NoGoat
    | OneGoat a
    | MoreGoats (GoatLord a) (GoatLord a) (GoatLord a) 

instance Functor GoatLord where 
    fmap _ NoGoat = NoGoat
    fmap f (OneGoat a) = OneGoat $ f a
    fmap f (MoreGoats a b c) = MoreGoats (ap a) (ap b) (ap c)
        where ap = fmap f

data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt = Halt 
    fmap f (Print str a) = Print str $ f a
    fmap f (Read f') = Read (f . f')

--- Applicative 

added :: Maybe Integer
added = (+3) <$> (lookup 3 $ zip [1,2,3] [4,5,6])

y :: Maybe Integer
y = lookup 3 $ zip [1, 2, 3] [4, 5, 6]

z :: Maybe Integer
z = lookup 2 $ zip [1, 2, 3] [4, 5, 6]

tupled :: Maybe (Integer, Integer) 
tupled = (,) <$> y <*> z

x :: Maybe Int
x = elemIndex 3 [1, 2, 3, 4, 5]
y' :: Maybe Int
y' = elemIndex 4 [1, 2, 3, 4, 5]

max' :: Int -> Int -> Int 
max' = max

maxed :: Maybe Int 
maxed = max' <$> x <*> y'

xs = [1, 2, 3] 
ys = [4, 5, 6]

x'' :: Maybe Integer
x'' = lookup 3 $ zip xs ys

y'' :: Maybe Integer
y'' = lookup 2 $ zip xs ys

summed =  sum <$> ( (,) <$> x'' <*> y'')

-- Applicatives
-----------------------

instance Applicative Identity where
    pure x = Identity x
    (<*>) (Identity f) (Identity a) = Identity $ f a

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where 
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant $ a <> b


data Bull =
      Fools
    | Twoo
    deriving (Eq, Show)

instance Arbitrary Bull where 
    arbitrary = frequency [ (1, return Fools) , (1, return Twoo) ]

instance Semigroup Bull where
    (<>) _ _ = Fools

instance Monoid Bull where 
    mempty = Fools

instance EqProp Bull where (=-=) = eq

-- quickBatch (monoid Twoo) // fail

-- xss = [("b", "w", 1 :: Int)]
type SSI = (String, String, Int)

applicativeTest :: List (String, String, Int)
applicativeTest = undefined

---

stops :: String 
stops = "pbtdkg"

vowels :: String 
vowels = "aeiou"

combos :: [a] -> [b] -> [c] -> [(a, b, c)] 
combos = liftA3 (,,)

kek = combos stops vowels stops

main :: IO ()
main = quickBatch $ applicative applicativeTest

