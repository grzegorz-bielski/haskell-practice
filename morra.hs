module Morra where

import           Control.Monad                  (when)
-- import           Control.Monad.State.Lazy       (evalState)
import           Control.Monad.Trans            (liftIO)
import           Control.Monad.Trans.State.Lazy (StateT (..), evalStateT, get,
                                                 gets, put)
-- import           System.Environment             (getArgs)
-- import           Date.Maybe                     (isNothing)
import           Data.Maybe                     (fromMaybe)
import           System.Random                  (StdGen, mkStdGen, randomIO,
                                                 randomR)
import           Text.Read                      (readMaybe)

-- StateT , IO

data PlayerType = User | AI deriving (Show, Eq)

data Player = Player {
    name       :: String,
    playerType :: PlayerType,
    vote       :: Maybe Int,
    act        :: Maybe Int
}

instance Show Player where
    show p = concat
        [ name p, " ", (show . playerType) p, " ",  (show . vote) p]


data GameStatus = Ongoing | End

data GameState = GameState {
    players :: [Player],
    status  :: GameStatus
}

type Game = StateT GameState IO ()

maxVote = 5

range :: (Num a, Enum a) => a -> [a]
range n = [1,2.. (n - 2)]

toInput :: String -> Maybe Int
toInput = readMaybe

takeUserInput :: (Int -> Bool) -> IO Int
takeUserInput pred = getLine >>= readInput >>= parseInput
    where
          readInput line =
            case toInput line of
                Nothing  -> continue
                Just val -> pure val

          parseInput vote =
            if pred vote then pure vote else continue

          continue = do
                putStrLn "Incorrect format"
                takeUserInput pred

takeAIInput :: Int -> IO Int
takeAIInput max = (fst . randomR (1, maxVote)) <$> mkStdGen <$> randomIO

takeInputs :: [Player] -> IO [Player]
takeInputs players =  sequenceA $ gatherVotes <$> players
    where gatherVotes player = let

            getInput pred aiMax = case playerType player of
                User -> takeUserInput pred
                AI   -> takeAIInput aiMax

            takeVote = getInput (const True) ((length players) * maxVote)
            takeAct = getInput (<= maxVote) maxVote

            in  do
                putStrLn "---"
                putStrLn $ (show . name) player
                putStrLn "---"

                putStrLn "Vote: "
                vote <- takeVote
                putStrLn $ show vote

                putStrLn "Act: "
                act <- takeAct
                putStrLn $ show act

                pure player { vote = Just vote, act = Just act }


getPlayersWithScore :: [Player] -> Int -> [Player]
getPlayersWithScore players actsSum =
    foldr (\player acc ->
        if (isValid player) then player : acc else acc
    )
    [] players
            where isValid player = fromMaybe False $ (==actsSum) <$> vote player


playGame :: Game
playGame = do
    statePlayers <- gets players
    playersWithInputs <- liftIO $ takeInputs statePlayers

    let actsSum = sum $ fromMaybe 0 <$> act <$> playersWithInputs
    let playersWithScore = getPlayersWithScore playersWithInputs actsSum

    liftIO $ putStrLn $ if (length playersWithScore) > 1 || (length playersWithScore) == 0
                        then "No winner, the sum was: " <> show actsSum
                        else "Winner is: " <> (show $ (name . head) playersWithScore)

    put $ GameState playersWithInputs Ongoing

    -- StateT $ \_ -> pure ((), GameState [] End)

main :: IO ()
main = do
    let players = (\f -> f Nothing Nothing) <$> [Player "Player 1" User, Player "Computer" AI]
    let game =  GameState players Ongoing

    evalStateT playGame game
