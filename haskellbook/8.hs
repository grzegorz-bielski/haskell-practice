module FromChapter8 where

dividedBy :: Integral a => a -> a -> Maybe (a, a)
dividedBy _ 0 = Nothing
dividedBy num denom = Just validResult
    where validResult = if isNegative then (negate $ fst result, snd result) else result
          isNegative = num < 0 || denom < 0
          result = go (abs num) (abs denom) 0
          go n d count
            | n < d = (count, n)
            | otherwise = go (n - d) d (count + 1)

-- 8.6
cattyConny :: String -> String -> String 
cattyConny x y = x ++ " mrow " ++ y

flippy = flip cattyConny
appedCatty = cattyConny "woops"
frappe = flippy "haha"
--
sumRec :: (Eq a, Num a) => a -> a
sumRec n = go 0 n
    where go curr count
            | count == 0 = curr
            | otherwise = go (curr + count) (count - 1)

recMult :: (Integral a) => a -> a -> a
recMult x y = if isNegative then negate result else result
    where result = go 0 0
          isNegative = x < 0 || y < 0
          xAbs = abs x
          yAbs = abs y
          go sum curr
            | curr == yAbs = sum
            | otherwise = go (sum + xAbs) (curr + 1)