{-# LANGUAGE FlexibleInstances #-}

module Main where

import           Control.Applicative      (liftA2, liftA3)
import           Control.Monad            (ap, join, (>=>))
import           Data.Char
import           Data.List                (elemIndex)
import           Data.Monoid
import           GHC.Arr
import           Lib
import           Test.QuickCheck
import           Test.QuickCheck.Checkers
import           Test.QuickCheck.Classes

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

---

data Pair a = Pair a a

instance Functor Pair where
    fmap f (Pair x y) = Pair (f x) (f y)

instance Applicative Pair where
    pure x = Pair x x
    (<*>) (Pair f g) (Pair x y) = Pair (f x) (g y)


instance Foldable Pair where
    foldMap f (Pair a b) = f a <> f b

instance Traversable Pair where
    traverse f (Pair a b) = Pair <$> f a <*> f b



---

data Two a b = Two a b

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

instance Monoid a => Applicative (Two a) where
    pure x = Two mempty x
    (<*>) (Two f g) (Two a b) = Two (f <> a) (g b)

---

data Three a b c = Three a b c

instance Functor (Three a b) where
    fmap f (Three x y z) = Three x y (f z)

instance (Monoid a, Monoid b) => Applicative (Three a b) where
    pure x = Three mempty mempty x
    (<*>) (Three _ _ f) (Three a b c) = Three a b $ f c

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c

instance Traversable (Three a b) where
    -- traverse f (Three a b c) = Three a b <$> f c
    traverse f (Three a b c) = fmap (Three a b) (f c)


---

data Three' a b = Three' a b b

instance Functor (Three' a) where
    fmap f (Three' x y z) = Three' x (f y) (f z)

instance (Monoid a) => Applicative (Three' a) where
    pure x = Three' mempty x x
    (<*>) (Three' _ g h) (Three' a b c) = Three' a (g b) (h c)

instance Foldable (Three' a) where
    foldMap f (Three' _ b c) = f b <> f c
    foldr f z (Three' _ b c) = f c (f b z)

instance Traversable (Three' a) where
    traverse f (Three' a b c) = Three' a <$> f b <*> f c

---

data Four a b = Four a a a b

instance Functor (Four a) where
    fmap f (Four a b c d) = Four a b c (f d)

---

data S n a = S (n a) a deriving (Eq, Show)

-- instance Functor (S n) where
--     fmap f (S x y) = S x y

--- cant implement
data Trivial = Trivial

---

data Option a = None | Some a deriving (Eq, Show)

instance Functor Option where
    fmap f None     = None
    fmap f (Some a) = Some $ f a

instance Applicative Option where
    pure = Some
    (<*>) (Some f) (Some a) = Some $ f a
    (<*>) None _            = None
    (<*>) _ None            = None

instance Foldable Option where
    foldMap f (Some a) = f a
    foldMap f None     = mempty

instance Traversable Option where
    traverse f (Some a) = Some <$> f a
    traverse f None     = pure $ None
---

data Sum' a b = First' a | Second b deriving (Eq, Show)

instance Functor (Sum' a) where
    fmap f (First' c) = First' c
    fmap f (Second b) = Second $ f b

instance Applicative (Sum' a) where
    pure a = Second a
    (<*>) (First' f) _          = First' f
    (<*>) _ (First' a)          = First' a
    (<*>) (Second f) (Second a) = Second $ f a

instance Monad (Sum' a) where
    (>>=) (First' a) _ = (First' a)
    (>>=) (Second a) f = f a

newtype Mu f = InF { outF :: f (Mu f) }

data D = D (Array Word Word) Int Int

---
data Company a b c = DeepBlue a c | Something b

instance Functor (Company a b) where
    fmap f (DeepBlue a c) = DeepBlue a (f c)
    fmap _ (Something b)  = Something b

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
    fmap _ (Desk a)  = Desk a
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
    | Cons a (List a)

instance Show a => Show (List a) where
    show a = "funny" ++ show a

instance Semigroup (List a) where
    (<>) Nil ys         = ys
    (<>) (Cons x xs) ys = Cons x $ xs <> ys

instance Monoid (List a) where
    mempty = Nil
    mappend = (<>)

instance Functor List where
    fmap _ Nil         = mempty
    fmap f (Cons x xs) = Cons (f x) (fmap f xs)

instance Applicative List where
    pure a = Cons a Nil
    (<*>) Nil _              = Nil
    (<*>) (Cons f fs) values = (f <$> values) <> (fs <*> values)

instance Foldable List where
    foldr _ a Nil         = a
    foldr f a (Cons x xs) = f x $ foldr f a xs

instance Traversable List where
    traverse _ Nil         = pure $ Nil
    traverse f (Cons x xs) = Cons <$> f x <*> traverse f xs

---

concat' :: List (List a) -> List a
concat' = foldr (<>) mempty

instance Monad List where
    return = pure
    (>>=) xs f = concat' $ f <$> xs

instance Arbitrary a => Arbitrary (List a) where
    arbitrary = (\a -> Cons a Nil) <$> arbitrary

instance Eq a => EqProp (List a) where
    (=-=) xs ys = (take' 1000 xs) `eq` (take' 1000 ys)

take' :: Int -> List a -> List a
take' 0 _           = Nil
take' _ Nil         = Nil
take' n (Cons x xs) = Cons x (take' (n - 1) xs)
--------

data Tree a =
      Empty
    | Leaf a
    | Node (Tree a) a (Tree a)

instance Functor Tree where
    fmap _ Empty = Empty
    fmap f (Leaf a) = Leaf $ f a
    fmap f (Node a b c) = Node (ap a) (f b) (ap c)
        where ap = fmap f

instance Foldable Tree where
    foldMap _ Empty        = mempty
    foldMap f (Leaf a)     = f a
    foldMap f (Node a b c) = foldMap f a <> f b <> foldMap f c

instance Traversable Tree where
    traverse f Empty        = pure Empty
    traverse f (Leaf a)     = Leaf <$> f a
    traverse f (Node a b c) = Node <$> traverse f a <*> f b <*> traverse f c


data TalkToMe a = Halt | Print String a | Read (String -> a)

instance Functor TalkToMe where
    fmap _ Halt          = Halt
    fmap f (Print str a) = Print str $ f a
    fmap f (Read f')     = Read (f . f')

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

instance Monad Identity where
    (>>=) (Identity a) f = f a

instance Foldable Identity where
    foldMap f (Identity a) = f a


--  traverse :: (Traversable t, Applicative f) => (a -> f b) -> t a -> f (t b)
instance Traversable Identity where
    traverse f (Identity a) = pure <$> f a

newtype Constant a b = Constant { getConstant :: a }

instance Functor (Constant a) where
    fmap _ (Constant a) = Constant a

instance Monoid a => Applicative (Constant a) where
    pure _ = Constant mempty
    (<*>) (Constant a) (Constant b) = Constant $ a <> b

instance Foldable (Constant a) where
    foldMap _ _ = mempty

instance Traversable (Constant a) where
    traverse f (Constant a) = pure $ Constant a


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

---

sayHi :: String -> IO String
sayHi str = putStrLn str >> getLine

readM :: Read a => String -> IO a
readM = pure . read

-- Kleisli composition
getAge :: String -> IO Int
getAge = sayHi >=> readM

askForAge :: IO Int
askForAge = getAge "Halko, give me your years"

---

data Nope a = DontCare

instance Functor Nope where
    fmap _ _ = DontCare

instance Applicative Nope where
    pure _ = DontCare
    (<*>) _ _ = DontCare

instance Monad Nope where
    (>>=) _ _ = DontCare

-- data EitheRev b a = Left'' a | Right'' b

-- instance Functor (EitheRev b) where
--     fmap f (Right'' b) = Right'' b
--     fmap f (Left'' a) = Left'' $ f a

-- instance Applicative (EitheRev b) where
--     pure a = Left'' a
--     (<*>) ()

j :: Monad m => m (m a) -> m a
-- j = join
j a = a >>= id

l1 :: Monad m => (a -> b) -> m a -> m b
l1 = fmap

l2 :: Monad m => (a -> b -> c) -> m a -> m b -> m c
-- l2 = liftA2
l2 f a b = f <$> a <*> b

a' :: Monad m => m a -> m (a -> b) -> m b
a' = flip ap
-- a' a f = f <*> a

meh :: Monad m => [a] -> (a -> m b) -> m [b]
meh [] _ = pure []
meh (x:xs) f = do
   cur <- f x
   rest <- meh xs f
   pure $ cur : rest

meh' :: Monad m => [a] -> (a -> m b) -> m [b]
meh' [] _     = pure []
meh' (x:xs) f = liftA2 (:) (f x) (meh xs f)

flipType :: (Monad m) => [m a] -> m [a]
flipType xs = meh xs id

---
-- Reader

cap :: [Char] -> [Char]
cap = fmap toUpper

rev :: [Char] -> [Char]
rev = reverse

tupled' :: [Char] -> ([Char], [Char])
tupled' = liftA2 (,) cap rev
-- tupled' = cap >>= (\a
--             -> rev >>= (\b
--                     -> pure (a, b)))
-- tupled' x = traverse (\([x, y]) -> pure (x, y)) [cap x, rev x]
-- tupled' = do
--     a <- cap
--     b <- rev
--     pure (a, b)

-- function instances:

-- instance Functor ((->) r) where
--     fmap = (.)

-- instance Applicative ((->) r) where
--     pure x = (\_ -> x)
--     (<*>) f g = \x -> f x (g x)

-- instance Monad ((->) r) where
--     (>>=) f g = \x -> g (f x) x

newtype Reader r a = Reader { runReader :: r -> a }

instance Functor (Reader r) where
    fmap f (Reader g) = Reader $ f . g

instance Applicative (Reader r) where
    pure a = Reader (\_ -> a)
    (<*>) (Reader f) (Reader g) = Reader $ \r -> (f r) (g r)

instance Monad (Reader r) where
    (>>=) (Reader f) g = Reader $ \r -> runReader (g (f r)) r

liftA2' :: Applicative f => (a -> b -> c) -> f a -> f b -> f c
liftA2' f t t' = f <$> t <*> t'

ask :: Reader a a
ask = Reader id

asks :: (r -> a) -> Reader r a
asks = Reader

---



---

maybeTraverse :: Maybe [Int]
maybeTraverse = traverse (\x -> case x of
                        Nothing -> pure 0
                        Just x  -> pure x
                ) [Just 1, Just 3, Nothing]

main :: IO ()
main = quickBatch $ applicative applicativeTest

