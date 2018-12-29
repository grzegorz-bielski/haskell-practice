import Control.Monad
import Data.List

solve :: (Num a, Read a, Floating a) => String -> a
solve = head . foldl resolve [] . words

resolve :: (Num a, Read a, Floating a) => [a] -> String -> [a]
resolve (x:y:ys) "*" = (x * y):ys
resolve (x:y:ys) "+" = (x + y):ys
resolve (x:y:ys) "-" = (y - x):ys
resolve (x:y:ys) "/" = (y / x):ys
resolve (x:y:ys) "^" = (y ** x):ys
resolve (x:xs) "ln" = log x:xs
resolve xs "sum" = [sum xs]
resolve xs numberString = read numberString:xs

solveMaybe :: (Num a, Read a, Floating a) => String -> Maybe a
solveMaybe str = foldM resolveMaybe [] (words str) >>= \[result] -> pure result

resolveMaybe :: (Num a, Read a, Floating a) => [a] -> String -> Maybe [a]
resolveMaybe (x:y:ys) "*" = pure $ (x * y):ys
resolveMaybe (x:y:ys) "+" = pure $ (x + y):ys
resolveMaybe (x:y:ys) "-" = pure $ (y - x):ys
resolveMaybe (x:y:ys) "/" = pure $ (y / x):ys
resolveMaybe (x:y:ys) "^" = pure $ (y ** x):ys
resolveMaybe (x:xs) "ln" = pure $ log x:xs
resolveMaybe xs "sum" = pure [sum xs]
resolveMaybe xs numberString = liftM (:xs) (readMaybe numberString)

readMaybe :: (Read a) => String -> Maybe a
readMaybe str = case reads str of
    [(x, "")] -> Just x
    otherwise -> Nothing