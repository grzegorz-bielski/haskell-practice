import Control.Monad

type KnightPos = (Int,Int)

-- with monads
potentialMoves :: KnightPos -> [KnightPos]
potentialMoves startingPos = do
    let (c,r) = startingPos
    (c',r') <- [(c+2,r-1), (c+2,r+1),(c-2,r-1),(c-2,r+1)
                ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)
               ]
    guard (c' `elem` [1..8] && r' `elem` [1..8])
    pure (c', r')

-- without monads
potentialMoves' :: KnightPos -> [KnightPos]  
potentialMoves' (c,r) = filter onBoard  
        [(c+2,r-1),(c+2,r+1),(c-2,r-1),(c-2,r+1)  
        ,(c+1,r-2),(c+1,r+2),(c-1,r-2),(c-1,r+2)  
        ]  
        where onBoard (c,r) = c `elem` [1..8] && r `elem` [1..8]
        
-- positionsFromMoves :: Int -> KnightPos -> [KnightPos]
-- positionsFromMoves n start = do

inThree :: KnightPos -> [KnightPos]
inThree start = pure start >>= potentialMoves >>= potentialMoves >>= potentialMoves

canInThree :: KnightPos -> KnightPos -> Bool
canInThree start end = end `elem` inThree start

inN :: Int -> KnightPos -> [KnightPos]
inN n = foldr (<=<) pure $ replicate n potentialMoves

inN' :: Int -> KnightPos -> [KnightPos]
inN' n start = pure start >>= foldr (<=<) pure (replicate n potentialMoves)

canInN :: Int -> KnightPos -> KnightPos -> Bool
canInN n start end = end `elem` inN n start