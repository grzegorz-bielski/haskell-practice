module FromChapter23 where

import           Control.Applicative (liftA2)
import           Control.Monad       (replicateM)
import           System.Random       (StdGen, mkStdGen, randomIO, randomR)

data Die =
      DieOne
    | DieTwo
    | DieThree
    | DieFour
    | DieFive
    | DieSix
    deriving (Eq, Show)

intToDie :: Int -> Die
intToDie n =
        case n of
            1 -> DieOne
            2 -> DieTwo
            3 -> DieThree
            4 -> DieFour
            5 -> DieFive
            6 -> DieSix

rollsToGet20 :: IO Int
rollsToGet20 = (rollsToGetN 20 . mkStdGen) <$> randomIO

rollsToGetN :: Int -> StdGen -> Int
rollsToGetN limit g = go 0 0 g
    where
        go :: Int -> Int -> StdGen -> Int
        go sum count gen
            | sum >= limit = count
            | otherwise =
                let (die, nextGen) = randomR (1, 6) gen
                in go (sum + die) (count + 1) nextGen

type DieCount = (Int, [Die])

rollsToGetN' :: Int -> StdGen -> DieCount
rollsToGetN' limit g = go 0 (0, []) g
    where
        go :: Int -> DieCount -> StdGen -> DieCount
        go sum (count, dies) gen
            | sum >= limit = (count, dies)
            | otherwise =
                let (dieValue, nextGen) = randomR (1, 6) gen
                    nextValue = sum + dieValue
                    nextCount = count + 1
                    collectedDies = (intToDie dieValue) : dies

                in go nextValue (nextCount, collectedDies) nextGen

---

newtype State' s a =
    State' { runState' :: s -> (a, s) }

instance Functor (State' s) where
    fmap f (State' g) = State' $ \s ->
        let a = (f . (fst . g)) s
        in (a, s)

instance Applicative (State' a) where
    pure a = State' $ \s -> (a, s)
    (<*>) (State' f) (State' g) = State' $ \s ->
        let a = liftA2 (flip ($)) (fst . g) (fst . f) $ s
        in (a, s)


instance Monad (State' a) where
    (>>=) (State' f) g = State' $ \s ->
        let a  = (g . fst . f) s
            a'  = (fst . runState' a) s
        in (a', s)


fizzBuzz :: Integer -> String
fizzBuzz n
    | n `mod` 15 == 0 = "FizzBuzz"
    | n `mod` 5  == 0 = "Buzz"
    | n `mod` 3  == 0 = "Fizz"
    | otherwise       = show n

main :: IO ()
main =
 mapM_ (putStrLn . fizzBuzz) [1..100]

---

get :: State' s s
get = State' $ \x -> (x, x)

put :: s -> State' s ()
put s = State' $ \_ -> ((), s)

exec :: State' s a -> s -> s
exec (State' sa) s = snd $ sa s

eval :: State' s a -> s -> a
eval (State' sa) s = fst $ sa s

modify :: (s -> s) -> State' s ()
modify f = State' $ \s -> ((), f s)
