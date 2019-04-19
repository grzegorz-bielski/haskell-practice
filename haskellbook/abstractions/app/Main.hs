module Main where

import Lib
import Test.QuickCheck

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

newtype Identity a = Identity a

instance Functor Identity where
    fmap f (Identity val) = Identity $ f val

data Pair a = Pair a a 

instance Functor Pair where 
    fmap f (Pair x y) = Pair (f x) (f y)
    
data Two a b = Two a b 

instance Functor (Two a) where
    fmap f (Two x y) = Two x (f y)

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

data Sum a b = First a | Second b deriving (Eq, Show)

instance Functor (Sum a) where
    fmap f (First c) = First c
    fmap f (Second b) = Second $ f b


main :: IO ()
main = putStrLn "test"
