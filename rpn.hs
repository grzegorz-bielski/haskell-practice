import Data.List

solve :: (Num a, Read a) => String -> a
solve = head . foldl resolve [] . words
    where resolve (x:y:ys) "*" = (x * y):ys
          resolve (x:y:ys) "+" = (x + y):ys
          resolve (x:y:ys) "-" = (y - x):ys
          resolve (x:y:ys) "/" = (y / x):ys
          resolve (x:y:ys) "^" = (y ** x):ys
          resolve (x:xs) "ln" = log x:xs
          resolve xs "sum" = [sum xs]
          resolve xs numberString = read numberString:xs