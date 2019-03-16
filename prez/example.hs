module Example where

import Control.Monad

constant = 5 :: Int

fun x = x * 4

twenty :: Int
twenty = fun 5

add :: Int -> Int -> Int
add x y = x + y

ten = add 5 5
addFive = add 5
twentyFive = addFive twenty

add' :: Num a => a -> a -> a
add' x y = x + y

(+>) x y = x + x + y

main :: IO ()
main = print $ twentyFive +> 5


--

factorial :: Integral a => a -> a
factorial 0 = 1
factorial n = n * factorial (n - 1)

-- or
factorial' n = product [1..n]

--

or' :: Foldable f => f Bool -> Bool
or' = foldr (||) False

any' :: (Foldable f, Functor f) => (a -> Bool) -> f a -> Bool
any' f = or' . fmap f


-- 

lexer :: Char -> String -> [String]
lexer char str = reverse $ go str []
    where go str acc = 
            let token = takeWhile (/=char) str
                rest = (dropWhile (==char) . dropWhile (/=char)) str
                soFar = (token : acc)
            in if rest == "" then soFar else go rest soFar

---

printUntilSpace :: IO ()
printUntilSpace = getChar >>= \c ->  
    if c /= ' ' 
    then putChar c >> printUntilSpace 
    else pure ()

printUntilSpace' :: IO ()
printUntilSpace' = do     
    c <- getChar  
    if c /= ' '  
        then do  
            putChar c  
            printUntilSpace'  
        else return ()


-- data Either' a b = Left' a | Right b 

data Maybe a = Just a | Nothing

sth :: Int -> Either String Int
sth a = if a > 5 then Left "za duo" else Right 5

sthh :: Either String Int -> String
sthh (Left a) = "koniec"
sthh (Right a) = show a


