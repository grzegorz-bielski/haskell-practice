import Data.List  

data Section = Section {
    getA :: Int,
    getB :: Int,
    getC :: Int
} deriving (Show)

type RoadSystem = [Section]

data Label = A | B | C deriving (Show)

type Path = [(Label, Int)]

heathrowToLondon :: RoadSystem  
heathrowToLondon = [Section 50 10 30, Section 5 90 20, Section 40 2 25, Section 10 8 0]

pathCost :: Path -> Int
pathCost path = sum $ fmap snd path

roadStep :: (Path, Path) -> Section -> (Path, Path)
roadStep (pathA, pathB) (Section a b c) = (newPathToA, newPathToB)
    where costA = pathCost pathA
          costB = pathCost pathB
          forwardCostToA = costA + a
          forwardCostToB = costB + b
          crossCostToA = costB + b + c
          crossCostToB = costA + a + c
          newPathToA = if forwardCostToA <= crossCostToA
                            then (A,a):pathA
                            else (C,c):(B,b):pathB
          newPathToB = if forwardCostToB <= crossCostToB
                            then (B,b):pathB
                            else (C,c):(A,a):pathA


optimalPath :: RoadSystem -> Path
optimalPath roadSystem = reverse $ 
    if sumUp bestAPath <= sumUp bestBPath 
        then bestAPath 
        else bestBPath
            where 
                (bestAPath, bestBPath) = foldl' roadStep ([],[]) roadSystem
                sumUp path = sum $ fmap snd path
                
groupsOf :: Int -> [a] -> [[a]]
groupsOf 0 _ = undefined
groupsOf _ [] = []
groupsOf n xs = take n xs : groupsOf n (drop n xs)

main = do
    contents <- getContents
    let threes = groupsOf 3 $ fmap read $ lines contents
        roadSystem = fmap (\[a,b,c] -> Section a b c) threes
        path = optimalPath roadSystem
        pathString = concat $ fmap (show . fst) path
        pathPrice = sum $ fmap snd path
    putStrLn $ "The best path: " ++ pathString
    putStrLn $ "The price is: " ++ show pathPrice