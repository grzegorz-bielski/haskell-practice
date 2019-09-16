module Roll where

import           Control.Monad.State (State, get, put, runState)
import qualified System.Random       as R


getRand :: R.Random a => (a, a) -> State R.StdGen a
getRand bounds = get
    >>= \g -> let (x, g') = R.randomR bounds g in put g' >> pure x

-- getRandomValue :: R.StdGen -> (Int, R.StdGen)
-- getRandomValue = R.getStdGen
--         >>= (fst . runState (getRand (1, 100)))
-- -- getRand' :: State StdGen Int
-- getRand' bounds = do
--     g <- get
--     let (x, g') = randomR bounds g
--     put g' >> pure x

