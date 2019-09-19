{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Control.Monad.IO.Class
import           Control.Monad.Trans.Class
import           Control.Monad.Trans.Reader
import           Data.IORef
import qualified Data.Map                   as M
import           Data.Maybe                 (fromMaybe)
import           Data.Text.Lazy             (Text)
import qualified Data.Text.Lazy             as TL
import           Debug.Trace                (trace)
import           Lib
import           System.Environment         (getArgs)
import           Web.Scotty.Trans

import           Lib

data Config = Config {
    counts :: IORef (M.Map Text Integer),
    prefix :: Text
}

type Scotty = ScottyT Text (ReaderT Config IO)
type Handler = ActionT Text (ReaderT Config IO)

countUp :: Text -> M.Map Text Integer -> (M.Map Text Integer, Integer)
countUp k m = (M.insert k val m, val)
    where maybeVal = M.lookup k m
          val = (fromMaybe 0 maybeVal) + 1

counter :: Config -> Text -> IO Integer
counter conf unprefixed = do
        (newMap, newValue) <- countUp key' <$> map'
        writeIORef ref' newMap
        pure newValue
        where key' = (prefix conf) <> unprefixed
              ref' = counts conf
              map' = readIORef ref'

app :: Scotty ()
app =
    get "/:key" $ do
        unprefixed <- param "key"
        conf <- lift ask
        val <- liftIO $ (TL.pack . show) <$> counter conf unprefixed

        html
            $ "<h1>Success! Count is: " <> val <> "</h1>"

main :: IO ()
main = do
    [prefixArg] <- getArgs
    counter <- newIORef M.empty
    let config = Config counter (TL.pack prefixArg)
        runR r = runReaderT r config
    scottyT 3001 runR app
