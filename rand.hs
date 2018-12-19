import System.Random

throwCoin :: StdGen -> (Bool, Bool)
throwCoin gen =
    (firstCoin, secondCoin)
    where (firstCoin, gen') = random gen
          (secondCoin, _) = random gen'

randoms' :: (RandomGen g, Random a) => g -> [a]
randoms' gen = let (value, gen') = random gen in value:randoms' gen'

randThrows n = take n $ randoms $ mkStdGen 11

diceThrow :: Int
diceThrow = fst $ randomR (1,6) $ mkStdGen 3434

diceThrow' :: Int -> IO ()
diceThrow' n = do
    gen <- getStdGen
    let getRand = head . take n . randomRs (1,6) :: (RandomGen g) => g -> Int
    putStrLn $ show $ getRand gen

main = diceThrow' 3