module FromChapter20 where

import           Data.Monoid (Product (..), Sum (..))

class Foldable' t where
    fold' :: Monoid m => t m -> m
    foldMap' :: Monoid m => (a -> m) -> t a -> m

sum :: (Foldable t, Num a) => t a -> a
sum t = getSum $ foldMap Sum t

product :: (Foldable t, Num a) => t a -> a
product t = getProduct $ foldMap Product t

elem' :: (Foldable t, Eq a) => a -> t a -> Bool
elem' v = foldr (\curr acc -> if acc then acc else curr == v) False

-- type :: (a -> a -> a)

extreme :: (Foldable t, Ord a) => (a -> a -> a) -> t a -> Maybe a
extreme f xs = foldr f' Nothing xs
    where f' x Nothing  = Just x
          f' x (Just y) = Just $ f x y

minimum' :: (Foldable t, Ord a) => t a -> Maybe a
minimum' = extreme max

maximum' ::  (Foldable t, Ord a) => t a -> Maybe a
maximum' = extreme min

null :: (Foldable t) => t a -> Bool
null = foldr (\_ _ -> False) True

length :: (Foldable t) => t a -> Int
length = foldr (\_ acc -> acc + 1) 0

toList :: (Foldable t) => t a -> [a]
toList = foldr (\curr acc -> curr : acc) []

fold :: (Foldable t, Monoid m) => t m -> m
fold = foldMap id
-- fold = foldr (\curr acc -> curr <> acc) mempty

foldMap'' :: (Foldable t, Monoid m) => (a -> m) -> t a -> m
foldMap'' f = foldr (\curr acc -> f curr <> acc) mempty

---
data Constant a b = Constant b

instance Foldable (Constant b) where
    foldMap _ _ = mempty

data Two a b = Two a b

instance Foldable (Two a) where
    foldMap f (Two a b) = f b

data Three a b c = Three a b c

instance Foldable (Three a b) where
    foldMap f (Three _ _ c) = f c


data Three' a b = Three' a b b

instance Foldable (Three' a) where
    foldMap f (Three' _ b b') = f b <> f b'

data Four' a b = Four' a b b b

instance Foldable (Four' a) where
    foldMap f (Four' _ x y z) = f x <> f y <> f z

filterF :: (Applicative f, Foldable t, Monoid (f a)) => (a -> Bool) -> t a -> f a
filterF p = foldMap (\x -> if p x then mempty else pure x)
