module FromChapter25 where

import           Control.Applicative (liftA2)

newtype Compose f g a =
    Compose { getCompose :: f (g a) }
    deriving (Eq, Show)

xs = [Just 1, Nothing]
sth = fmap (+1) (Compose xs)


instance (Functor f, Functor g) => Functor (Compose f g) where
    fmap f (Compose fga) = Compose $ (fmap . fmap) f fga

instance (Applicative f, Applicative g) => Applicative (Compose f g) where
    pure a = Compose $ (pure . pure) a
    (<*>) (Compose fgf) (Compose fga) = Compose $ liftA2 (<*>) fgf fga

instance (Monad f, Monad g) => Monad (Compose f g) where
    (>>=) (Compose fga) ffgb = undefined -- not possible

instance (Foldable f, Foldable g) => Foldable (Compose f g) where
    foldMap f (Compose fga) = (foldMap . foldMap) f fga

instance (Traversable f, Traversable g) => Traversable (Compose f g) where
    traverse f (Compose fga) = Compose <$> (traverse . traverse) f fga

---

class Bifunctor p where
    bimap :: (a -> b) -> (c -> d) -> p a c -> p b d
    bimap f g = first f . second g

    first :: (a -> b) -> p a c -> p b c
    first f = bimap f id

    second :: (b -> c) -> p a b -> p a c
    second = bimap id

data Deux a b = Deux a b

instance Bifunctor Deux where
    bimap f g (Deux a b) = Deux (f a) (g b)

data Const a b = Const a

instance Bifunctor Const where
    bimap f _ (Const a) = Const $ f a

data Drei a b c = Drei a b c

instance Bifunctor (Drei a) where
    bimap f g (Drei a b c) = Drei a (f b) (g c)

data SuperDrei a b c = SuperDrei a b

instance Bifunctor (SuperDrei a) where
    bimap f _ (SuperDrei a b) = SuperDrei a (f b)

data SemiDrei a b c = SemiDrei a

instance Bifunctor (SemiDrei a) where
    bimap _ _ (SemiDrei a) = SemiDrei a

data Either' a b = Left' a | Right' b

instance Bifunctor Either' where
    bimap f _ (Left' a)  = Left' $ f a
    bimap _ g (Right' a) = Right' $ g a

---
-- transformers
newtype Identity a = Identity { runIdentity :: a } deriving (Eq, Show)
newtype IdentityT f a = IdentityT { runIdentityT :: f a } deriving (Eq, Show)

instance Functor Identity where
    fmap f (Identity a) = Identity $ f a

instance (Functor m) => Functor (IdentityT m) where
    fmap f (IdentityT fa) = IdentityT $ fmap f fa

instance Applicative Identity where
    pure = Identity
    (<*>) (Identity f) (Identity a) = Identity $ f a

instance (Applicative m) => Applicative (IdentityT m) where
    pure x = IdentityT $ pure x
    (<*>) (IdentityT fab) (IdentityT fa) = IdentityT $ fab <*> fa

instance Monad Identity where
    (>>=) (Identity a) f = f a

instance (Monad m) => Monad (IdentityT m) where
    (>>=) (IdentityT ma) f = IdentityT $ ma >>= runIdentityT . f

---

most :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a)] -> [Maybe (Identity b) ]
most = three . two . one where
    one    = ((fmap . fmap) (<*>)) -- 1. Identity
    two    = fmap (<*>)            -- 2. Maybe
    three  = (<*>)                 -- 3. Array

innerMost :: [Maybe (Identity (a -> b))] -> [Maybe (Identity a -> Identity b)]
innerMost = (fmap . fmap) (<*>)

innerMost' :: Maybe (Identity (a -> b)) -> Maybe (Identity a -> Identity b)
innerMost' = fmap (<*>)


innerMost'' :: Identity (a -> b) -> Identity a -> Identity b
innerMost'' = (<*>)

innerMost''' :: (a -> b) -> Identity a -> Identity b
innerMost''' = (<$>)

innerMost'''' :: (a -> b) -> a -> b
innerMost'''' = ($)
