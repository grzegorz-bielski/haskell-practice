module Listt where

data List a =
    Nil
    | Cons a (List a) deriving (Show, Eq)

-- instance Show a => Show (List a) where
    -- show a = "funny" ++ show a

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

