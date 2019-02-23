import Control.Monad
import Control.Monad.Writer
import Control.Monad.State
import System.Random
import Data.Ratio

-- (<>) :: Semigroup a => a -> a -> a
-- fmap :: (Functor f) => (a -> b) -> f a -> f b  
-- (<*>) :: (Applicative f) => f (a -> b) -> f a -> f b  
-- (>>=) :: (Monad m) => m a -> (a -> m b) -> m b  

applyMaybe :: Maybe a -> (a -> Maybe b) -> Maybe b
applyMaybe Nothing f = Nothing
applyMaybe (Just x) f = f x 

justFive = Just 4 `applyMaybe` \x -> Just (x+1)
nothing = Nothing `applyMaybe` \x -> Just (x ++ "sth")
nothing' = Just 1 `applyMaybe` \x -> if x > 2 then Just x else Nothing

-- // normal application
-- (-:) :: a -> (a -> b) -> b
-- x -: f = f x
-- // monadic application
-- (>>=) :: m a -> (a -> m b) -> m b

-- class Monad m where
--     return :: a -> m a
--     (>>=) :: m a -> (a -> m b) -> m b
--     (>>) :: m a -> m b -> m b
--     x >> y = x >>= \_ -> y
--     fail :: String -> m a
--     fail msg = error msg

-- instance Monad Maybe where  
--     return x = Just x  
--     Nothing >>= f = Nothing  
--     Just x >>= f  = f x  
--     fail _ = Nothing  

type Birds = Int
type Pole = (Birds, Birds)

maxBirds :: Int
maxBirds = 4

landLeft :: Birds -> Pole -> Maybe Pole
landLeft n (left, right)
    | abs ((left + n) - right) < maxBirds = Just (left + n, right)
    | otherwise = Nothing

landRight :: Birds -> Pole -> Maybe Pole
landRight n (left, right)
    | abs (left - (right + n)) < maxBirds = Just (left, right + n)
    | otherwise = Nothing

landingSeq = return (0,0) >>= landRight 2 >>= landLeft 2 >>= landRight 2  
landingErr = return (0,0) >>= landLeft 1 >> Nothing >>= landRight 1

-- nested >>=
foo :: Maybe String
foo = Just 3   >>= (\x ->
      Just "!" >>= (\y ->
      Just (show x ++ y )))

-- do notation, syntax for chaining monadic values
foo' :: Maybe String
foo' = do
    x <- Just 3
    y <- Just "!"
    Just (show x ++ y)

landingSeq' :: Maybe Pole
landingSeq' = do
    start <- pure (0,0)
    first <- landLeft 2 start
    -- Nothing // just like >> Nothing
    second <- landRight 2 first
    landLeft 1 second

justH :: Maybe Char
justH = do
    (x:xs) <- Just "hello"
    return x

justH' = Just "hello" >>= \(x:xs) -> pure x

-- list comprehensions are just syntactic sugar for using lists as monads
tuples = [1,2] >>= \n -> ['a','b'] >>= \ch -> pure (n,ch)
tuples' = do
    n <- [1,2]
    ch <- ['a', 'b']
    pure (n,ch)
    
-- monad as monoids
class Monad m => MonadPlus' m where
    -- aka mempty
    mzero' :: m a
    -- aka mappend
    mplus' :: m a -> m a -> m a

instance MonadPlus' [] where
    mzero' = []
    mplus' = (++)

guard' :: (MonadPlus' m) => Bool -> m ()
guard' True = pure ()
guard'  False = mzero'

-- monad laws
-- 1. left identity
-- return x >>= f = f x
-- 2. right identity
-- - m >>= return
-- 3. associativity
-- m >>= f) >>= g = m >>= (\x -> f x >>= g)

-- function composition:
-- (.) :: (b -> c) -> (a -> b) -> (a -> c)  
-- f . g = (\x -> f (g x))

-- monadic composition: 
-- (<=<) :: (Monad m) => (b -> m c) -> (a -> m b) -> (a -> m c)  
-- f <=< g = (\x -> g x >>= f) 

f x = [x,-x]
g x = [x*3, x*2]
h = f <=< g

applyLog :: (Monoid m) => (a,m) -> (a -> (b,m)) -> (b,m)  
applyLog (x,log) f = let (y,newLog) = f x in (y,log `mappend` newLog)  

-- newtype Writer' w a = Writer' { runWriter' :: (a, w) }

-- instance (Monoid w) => Monad (Writer w) where
--     return x = Writer' (x, mempty)
--     (Writer' (x,v)) >>= f = let (Writer' (y, v')) = f x in Writer' (y, v `mappend` v')

resStr = runWriter (pure 3 :: Writer String Int)
resSum = runWriter (pure 3 :: Writer (Sum Int) Int)

logNum :: Int -> Writer [String] Int
logNum x = writer (x, ["Got number: " ++ show x])

multLog :: Writer [String] Int
multLog = logNum 3 >>= \x -> logNum 5 >>= \y -> pure (x*y)

multLog' :: Writer [String] Int
multLog' = do
    a <- logNum 3
    b <- logNum 5
    pure (a*b)

-- greatest common divisor
gcd' :: Int -> Int -> Writer [String] Int
gcd' a b
    | b == 0 = tell ["Finished with " ++ show a] >> pure a
    | otherwise = tell [show a ++ " mod " ++ show b ++ " = " ++ show res] >> gcd' b res
        where res = a `mod` b

gcd'' :: Int -> Int -> Writer [String] Int  
gcd'' a b  
    | b == 0 = do  
        tell ["Finished with " ++ show a]  
        return a  
    | otherwise = do  
        tell [show a ++ " mod " ++ show b ++ " = " ++ show (a `mod` b)]  
        gcd'' b (a `mod` b)
        
newtype DiffList a = DiffList { getDiffList :: [a] -> [a] }

toDiffList :: [a] -> DiffList a
toDiffList xs = DiffList (xs++)

fromDiffList :: DiffList a -> [a]
fromDiffList (DiffList f) = f []

--  instead of mappend
instance Semigroup (DiffList a) where
    (<>) (DiffList f) (DiffList g) = DiffList (\xs -> f (g xs))  

instance Monoid (DiffList a) where
    mempty = DiffList (\xs -> [] ++ xs)

-- function Monad
-- instance Monad ((->) r) where  
--     return x = \_ -> x  
--     h >>= f = \w -> f (h w) w  

-- applicative style
ff = (+) <$> (*2) <*> (+10)
-- monadic style
ff' = do  
    a <- (*2)  
    b <- (+10)  
    return (a+b)
ff'' = (*2) >>= \x -> (+10) >>= \y -> pure (x+y)


-- // state

type Stack = [Int]

pop :: Stack -> (Int, Stack)
pop (x:xs) = (x,xs)

push :: Int -> Stack -> ((), Stack)
push a xs = ((),a:xs)

stackManip :: Stack -> (Int, Stack)  
stackManip stack = let  
    ((),newStack1) = push 3 stack  
    (a ,newStack2) = pop newStack1  
    in pop newStack2  

-- newtype State' s a = State' { runState :: s -> (a,s) } 

-- instance Monad (State' s) where
--     return x = State' $ \s -> (x,s)
--     (>>=) (State' h) f = State' $ \s -> let (a, newState) = h s
--                                             (State' g) = f a
--                                         in g newState

pop' :: State Stack Int
pop' = state pop

push' :: Int -> State Stack ()
push' a = state $ push a

-- // unbinded value is not used, so >> is ok
stackManip' :: State Stack Int
stackManip' = push' 3 >> pop' >> pop'

stackManip'' :: State Stack Int
stackManip'' = do  
    push' 3  
    a <- pop'  
    pop'
    
getPutStack :: State Stack ()
getPutStack = get >>= \stackNow -> put $ if stackNow == [1,2,3] then [8,3,1] else [9,2,1]

randomSt :: (RandomGen g, Random a) => State g a  
randomSt = state random 

threeCoins :: State StdGen (Bool,Bool,Bool)  
threeCoins = do  
    a <- randomSt  
    b <- randomSt  
    c <- randomSt  
    return (a,b,c)

coin :: Int -> StdGen -> [Bool]
coin n = fst . runState (sequence $ replicate n (state random))

coinThrow :: Int -> IO [Bool]
coinThrow n = newStdGen >>= \g -> pure (coin n g)

-- instance (Error e) => Monad (Either e) where  
--     return x = Right x   
--     Right x >>= f = f x  
--     Left err >>= f = Left err  
--     fail msg = Left (strMsg msg)  

left' =  Left "boom" >>= \x -> return (x+1)  
right' = Right 3 >>= \x -> return (x + 100) :: Either String Int

-- m >>= f = join (fmap f m) // often
join' :: (Monad m) => m (m a) -> m a
join' mm = mm >>= id

statefulList = snd $ runState (join (state $ \s -> (push' 10,1:2:s))) [0,0,0]

keepBelow :: Int -> Int -> Writer [String] Bool
keepBelow n x
    | x < n = tell ["Keeping " <> show x] >> pure True
    | otherwise = tell [show x <> " is too large, throwing it away"] >> pure False

keepBelow4 = keepBelow 4

-- filterM
below4 = fst $ runWriter $ filterM keepBelow4 [9,1,5,2,10,3]

powerset :: [a] -> [[a]]
powerset = filterM (\x -> [True, False])

-- foldM
smallOrNothing :: Int -> Int -> Maybe Int
smallOrNothing acc x
    | x > 9 = Nothing
    | otherwise = Just (acc + x)

below9 xs = foldM smallOrNothing 0 xs

foldComposition :: (Foldable a) => a (x -> x) -> x -> x
foldComposition funcs = foldr (.) id funcs


-- probabiliy lists
ratRes :: Ratio Int
ratRes = 1%4 + 1%4

newtype Prob a = Prob { getProb :: [(a,Rational)] } deriving Show

instance Functor Prob where
    fmap f (Prob xs) = Prob $ map (\(x,p) -> (f x, p)) xs

instance Applicative Prob where
    pure x = Prob [(x, 1%1)]
    (<*>) = ap

instance Monad Prob where
    return x = Prob [(x, 1%1)]
    m >>= f = probFlatten (fmap f m)
    fail _ = Prob []

data Coin = Heads | Tails deriving (Show, Eq)

probFlatten :: Prob (Prob a) -> Prob a
probFlatten (Prob xs) = Prob $ concat $ fmap multAll xs
    where multAll (Prob innerxs, p) = fmap (\(x,r) -> (x,p*r)) innerxs

coin' :: Prob Coin
coin' = Prob [(Heads, 1%2), (Tails, 1%2)]

loadedCoin :: Prob Coin
loadedCoin = Prob [(Heads, 1%10), (Tails, 9%10)]

flipThree :: Prob Bool  
flipThree = do  
    a <- coin'  
    b <- coin'  
    c <- coin'
    return (all (==Tails) [a,b,c])  

flipN :: Int -> Prob Bool  
flipN n = do
    arr <- sequence $ replicate n coin'
    return (all (==Tails) arr)
    
flipN' :: Int -> Prob Bool   
flipN' n = (sequence $ replicate n coin') >>= \coins -> pure $ all (==Tails) coins