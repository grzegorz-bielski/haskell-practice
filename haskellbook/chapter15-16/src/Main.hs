{-# OPTIONS_GHC -fwarn-incomplete-patterns #-}

module Main where

import Test.QuickCheck
import Data.Semigroup

data Optional a = None | Some a deriving (Eq, Show)

instance Semigroup a => Semigroup (Optional a) where
  (Some a) <> (Some b) = Some $ a <> b
  (Some a) <> None = Some a
  None <> (Some a) = Some a
  _ <> _ = None

instance Monoid a => Monoid (Optional a) where
  mempty = None
  mappend = (<>)

semigroupAssoc :: (Eq m, Semigroup m) => m -> m -> m -> Bool
semigroupAssoc a b c = (a <> (b <> c)) == ((a <> b) <> c)

monoidLeftIdentity :: (Eq m, Monoid m) => m -> Bool
monoidLeftIdentity a = (mempty <> a) == a

monoidRightIdentity :: (Eq m, Monoid m) => m -> Bool
monoidRightIdentity a = (a <> mempty) == a

newtype First' a =
  First' { getFirst' :: Optional a } deriving (Eq, Show)

instance Semigroup (First' a) where
  First' (Some a) <> _ = First' (Some a)
  First' None <> First' (Some a) = First' (Some a)
  _ <> _ = First' None

instance Monoid (First' a) where
  mempty = First' None
  mappend = (<>)

instance Arbitrary a => Arbitrary (First' a) where
  arbitrary = do
    a <- arbitrary
    frequency [ (1, pure (First' None))
              , (1, pure (First' (Some a)))]

firstMappend :: First' a -> First' a -> First' a
firstMappend = mappend

checkFirstAssoc = quickCheck (semigroupAssoc :: First' String -> First' String -> First' String -> Bool)
checkFirstLeftIdentity = quickCheck (monoidLeftIdentity :: First' String -> Bool)
checkFirstRightIdentity = quickCheck (monoidRightIdentity :: First' String -> Bool)

testFirstLaws :: IO ()
testFirstLaws = do
  checkFirstAssoc
  checkFirstLeftIdentity
  checkFirstRightIdentity

---

newtype Identity a = Identity a deriving (Eq, Show)

instance Semigroup a => Semigroup (Identity a) where
  (Identity a) <> (Identity b) = Identity (a <> b)

instance Monoid a => Monoid (Identity a) where
  mempty = Identity mempty
  mappend = (<>)

instance Arbitrary a => Arbitrary (Identity a) where
  arbitrary = arbitrary >>= (\a -> pure $ Identity a)

checkIdLeftId = quickCheck (monoidLeftIdentity :: Identity String -> Bool)
checkIdRighttId = quickCheck (monoidRightIdentity :: Identity String -> Bool)
checkIdentityAssoc = quickCheck (semigroupAssoc :: Identity String -> Identity String -> Identity String -> Bool)

---

data Two a b = Two a b deriving (Eq, Show)

instance (Semigroup a, Semigroup b) => Semigroup (Two a b) where
  (Two a b) <> (Two c d) = Two (a <> c) (b <> d)

instance (Monoid a, Monoid b) => Monoid (Two a b) where
  mempty = Two mempty mempty
  mappend = (<>)

instance (Arbitrary a, Arbitrary b) => Arbitrary (Two a b) where
  arbitrary = do
    a <- arbitrary
    b <- arbitrary
    pure $ Two a b

type TwoSemigroupAssoc = Two String (Product Int) -> Two String (Product Int) -> Two String (Product Int) -> Bool

checkTwosAssoc = quickCheck (semigroupAssoc :: TwoSemigroupAssoc)
checkTwosLeftId = quickCheck (monoidLeftIdentity :: Two String (Product Int) -> Bool)
checkTwosRightId = quickCheck (monoidRightIdentity :: Two String (Product Int) -> Bool)

--

newtype BoolConj = BoolConj Bool deriving (Eq, Show)

instance Semigroup BoolConj where
  (BoolConj True) <> _ = BoolConj True
  _ <> (BoolConj True) = BoolConj True
  _ <> _ = BoolConj False

instance Monoid BoolConj where
  mempty = (BoolConj False)

instance Arbitrary BoolConj where
  arbitrary = arbitrary >>= (\a -> pure $ BoolConj a)

type BoolConjAssoc = BoolConj -> BoolConj -> BoolConj -> Bool

checkBoolConjAssoc = quickCheck (semigroupAssoc :: BoolConjAssoc)

---

data Or a b = Fst a | Snd b deriving (Eq, Show)

instance Semigroup (Or a b) where
  (Fst a) <> (Fst b) = Fst b
  (Fst a) <> (Snd b) = Snd b
  (Snd a) <> _ = Snd a

instance (Arbitrary a, Arbitrary b) => Arbitrary (Or a b) where
  arbitrary = do 
    a <- arbitrary
    b <- arbitrary
    frequency [(1, pure $ Fst a), (1, pure $ Snd b )]

type OrAssoc = Or String Bool -> Or String Bool -> Or String Bool -> Bool

checkOrAssoc = quickCheck (semigroupAssoc :: OrAssoc)

---

newtype Combine a b = Combine { unCombine :: (a -> b) }

instance Semigroup b => Semigroup (Combine a b) where
  Combine f <> Combine g = Combine (f <> g)

instance Monoid b => Monoid (Combine a b) where
  mempty = Combine (id . mempty)
  mappend = (<>)

-- instance (Eq b, Eq a) => Eq (Combine a b) where
--   (==) (Combine f) (Combine f) = a == c && b && d

-- checkLeftId = quickCheck (monoidLeftIdentity :: Combine Int String -> Bool)

instance (CoArbitrary a, Arbitrary b) => Arbitrary (Combine a b) where
  arbitrary = arbitrary >>= (\f -> pure $ Combine f)

f = Combine $ \n -> Sum (n + 1)
g = Combine $ \n -> Sum (n - 1)

zeroSum = unCombine (f <> g) $ 0

-- instance Arbitrary b =>

-- uh...

---

newtype Comp a = Comp { unComp :: (a -> a) }

instance Semigroup a => Semigroup (Comp a) where
 Comp a <> Comp b = Comp (a <> b)

instance Monoid a => Monoid (Comp a) where
  mempty = Comp id

--

newtype Mem s a =
  Mem {
    runMem :: s -> (a,s)
  }


instance Semigroup a => Semigroup (Mem s a) where
  Mem f <> Mem g = Mem combineMem
    where combineMem x = (a <> c, d)
            where (a, b) = g x
                  (c, d) = f b

instance Monoid a => Monoid (Mem s a) where
  mempty = Mem $ \s -> (mempty, s)
  mappend = (<>)



main = testFirstLaws
