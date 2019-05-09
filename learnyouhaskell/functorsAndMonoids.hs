import           Control.Applicative
import qualified Data.Foldable       as F
import           Data.Monoid

-- functor laws:

-- 1. fmap id = id
-- if we map the `id` function over a functor, the functor that we get back
-- should be the same as the original functor
-- 2. fmap (f . g) = fmap f . fmap g
-- composing two functions and then mapping the resulting function over
-- a functor should be the same as first mapping one function over
-- the functor and then mapping the other one

-- applicative functors laws:

-- 1. pure f <*> x = fmap f x          (important!!)
-- 2. pure id <*> v = v
-- 3. pure (.) <*> u <*> v <*> w = u <*> (v <*> w)
-- 4. pure f <*> pure x = pure (f x)
-- 5. u <*> pure y = pure ($ y) <*> u

-- class (Functor f) => Applicative' fu where
--     pure :: a -> f a
--     (<*>) :: f (a -> b) -> f a -> f b

-- instance Applicative' Maybe where
--     pure = Just
--     Nothing <*> _ = Nothing
--     (Just f) <*> sth = fmap f sthr

(Just val) = pure (+) <*> Just 3 <*> Just 5
name = (++) <$> Just "first" <*> Just "second"
everyCombination = (*) <$> [2,5,10] <*> [8,10,11]
threes = (\x y z -> [x,y,z]) <$> (+3) <*> (*2) <*> (/2) $ 5
zipped = getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList (repeat 100)

myAction :: IO String
myAction = (++) <$> getLine <*> getLine

liftA2' :: (Applicative f) => (a -> b -> c) -> f a -> f b -> f c
liftA2' f a b = f <$> a <*> b

sequenceA' :: (Applicative f) => [f a] -> f [a]
sequenceA' = foldr (liftA2' (:)) (pure [])

-- Just [3, 2, 1]
res = sequenceA' [Just 3, Just 2, Just 1]

newtype ZipList a = ZipList' { getZipList' :: [a] }

newtype Pair b a = Pair { getPair :: (a,b)}

instance Functor (Pair c) where
    fmap f (Pair (x,y)) = Pair (f x, y)

-- A monoid is when you have an associative binary function and a value which acts as an identity with respect to that function

-- Monoid laws:
-- 1. mempty `mappend` x = x
-- 2. x `mappend` mempty = x
-- 3. (x `mappend` y) `mappend` z = x `mappend` (y `mappend` z)

class Monoid' m where
    mempty' :: m
    mappend' :: m -> m -> m
    mconcat' :: [m] -> m
    mconcat' = foldr mappend' mempty'

-- eg. Product: binary function: *, identity value: 1
newtype Product' a = Product' { getProduct' :: a }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Num a => Monoid' (Product' a) where
    mempty' = Product' 1
    Product' x `mappend'` Product' y = Product' (x * y)

-- or Sum: binary function: +, identity value: 0
-- -//-

newtype Any' = Any' { getAny' :: Bool }
    deriving (Eq, Ord, Read, Show, Bounded)

instance Monoid' Any' where
    mempty' = Any' False
    Any' x `mappend'` Any' y = Any' (x || y)

-- All -//-
instance Monoid' Ordering where
    mempty' = EQ
    LT `mappend'` _ = LT
    EQ `mappend'` y = y
    GT `mappend'` _ = GT

lengthCompare :: String -> String -> Ordering
lengthCompare x y = (length x `compare` length y) `mappend'`
                    (vowels x `compare` vowels y) `mappend`
                    (x `compare` y)
    where vowels = length . filter (`elem` "aeiou")

-- Foldable
reduced = F.foldl (*) 1 [1,2,3]
reduced' = F.foldl (||) False $ Just True

data Tree a = Empty | Node a (Tree a) (Tree a) deriving (Show, Read, Eq)

instance F.Foldable Tree where
    foldMap f Empty = mempty
    foldMap f (Node x l r) = F.foldMap f l `mappend`
                             f x           `mappend`
                             F.foldMap f r

testTree = Node 5
            (Node 3
                (Node 1 Empty Empty)
                (Node 6 Empty Empty)
            )
            (Node 9
                (Node 8 Empty Empty)
                (Node 10 Empty Empty)
            )

nodesSum = F.foldl (+) 0 testTree

isBiggerThan :: Int -> Bool
isBiggerThan n = getAny $ F.foldMap (\x -> Any $ x == n) testTree
