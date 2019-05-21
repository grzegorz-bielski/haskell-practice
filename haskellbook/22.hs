module ReaderPractice where

import           Control.Applicative
import           Data.Maybe

x = [1, 2, 3]
y = [4, 5, 6]
z = [7, 8, 9]

-- lookup :: Eq a => a -> [(a, b)] -> Maybe b

xs :: Maybe Integer
xs = lookup 3 $ zip x y

ys :: Maybe Integer
ys = lookup 6 $ zip y z

zs :: Maybe Integer
zs = lookup 4 $ zip x y

z' :: Integer -> Maybe Integer
z' n = lookup n $ zip x z

x1 :: Maybe (Integer, Integer)
x1 = liftA2 (,) xs ys

x2 :: Maybe (Integer, Integer)
x2 = liftA2 (,) ys zs

x3 :: Integer -> (Maybe Integer, Maybe Integer)
x3 n = let x = z' n in (x, x)

summed :: Num c => (c, c) -> c
summed = uncurry (+)

bolt :: Integer -> Bool
bolt = liftA2 (&&) (>3) (<8)

fromMaybe' :: a -> Maybe a -> a
fromMaybe' v Nothing  = v
fromMaybe' v (Just a) = a

sequA :: Integral a => a -> [Bool]
sequA m = sequenceA [(>3), (<8), even] m

s' = summed <$> ((,) <$> xs <*> ys)

-- kek = sequA $ fromMaybe' 0 s'

(|>) :: a -> (a -> b) -> b
(|>) x f = f x

(<|) = ($)

main :: IO ()
main = do
    print $ foldr (&&) True $ sequA 10

    print $ sequA $ fromMaybe' 0 s'
    fromMaybe' 0 s' |> sequA |> print

    fromMaybe' 0 ys |> bolt |> print
