module FromChapter26 where

import           Control.Monad.Reader       (Reader (..), reader)
import qualified Control.Monad.Reader       as R
import           Control.Monad.Trans.Except (ExceptT (..))
import           Data.Functor.Identity      (Identity (..))

-- Additional structure, `m` is wrapped around a value

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
     -- apply f on value inside Maybe, lifting over `m` and Maybe, then wrap it with MaybeT
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma

instance (Applicative m) => Applicative (MaybeT m) where
    -- pure for `m` and underlying Maybe, then wrapping it with MaybeT
    pure a = MaybeT $ (pure . pure) a
    -- (<*>) (MaybeT fab) (MaybeT ma) = MaybeT $ ((<*>) <$> fab <*> ma)
    (<*>) (MaybeT fab) (MaybeT ma) = MaybeT $ (two . one) fab ma where
        one = fmap (<*>) -- apply f in wrapped Maybe
        two = (<*>)      -- apply f in m

instance (Monad m) => Monad (MaybeT m) where
    (>>=) (MaybeT ma) f = MaybeT $ ma >>=
        \v -> case v of
                Nothing -> pure Nothing         -- m (Maybe a)
                Just x  -> (runMaybeT . f) x    -- f :: a -> MaybeT m b
                                                -- runMaybeT :: m (Maybe a)
                                                -- (runMaybeT . f) :: a -> m (Maybe a)

instance MonadTrans MaybeT where
    lift = MaybeT . fmap Just

instance (MonadIO m) => MonadIO (MaybeT m) where
    liftIO = lift . liftIO


sth = MaybeT $ Left (Just 1)
sth' = MaybeT $ [Just 1, Just 4]
kek = (+1) <$> sth'
kek' = runMaybeT kek

sth'' = MaybeT $ Just Nothing
---

example :: MaybeT (Either a) Int
example = (MaybeT . Right . Just) 1

---

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }

instance (Functor m) => Functor (EitherT e m) where
    fmap f (EitherT mea) = EitherT $ (fmap . fmap) f mea

instance (Applicative m) => Applicative (EitherT e m) where
    pure a = EitherT $ (pure . pure) a
    (<*>) (EitherT fmea) (EitherT mea) = EitherT $ (<*>) <$> fmea <*> mea

instance (Monad m) => Monad (EitherT e m) where
    (>>=) (EitherT mea) f = EitherT $ mea >>=
            \v -> case v of
                Left e  -> pure $ Left e
                Right a -> (runEitherT . f) a

instance MonadTrans (EitherT e) where
    lift = EitherT . fmap Right

instance (MonadIO m) => MonadIO (EitherT e m) where
    liftIO = lift . liftIO

---

newtype ReaderT r m a = ReaderT { runReaderT :: r -> m a }

instance (Functor m) => Functor (ReaderT r m) where
    fmap f (ReaderT rma) = ReaderT $ (fmap . fmap) f rma

instance (Applicative m) => Applicative (ReaderT r m) where
    pure a = ReaderT $ (pure . pure) a
    (<*>) (ReaderT fmab) (ReaderT rma) = ReaderT $ (<*>) <$> fmab <*> rma

instance (Monad m) => Monad (ReaderT r m) where
    (>>=) (ReaderT rma) f = ReaderT $
            \r -> rma r >>= \a -> runReaderT (f a) r

instance MonadTrans (ReaderT r) where
    lift m = ReaderT $ const m

instance (MonadIO m) => MonadIO (ReaderT r m) where
    liftIO = lift . liftIO
---

newtype StateT s m a = StateT { runStateT :: s -> m (a,s)}

instance (Functor m) => Functor (StateT s m) where
    fmap f (StateT sma) = StateT $
            \v -> (\(a, s) -> (f a, s)) <$> sma v

instance (Monad m) => Applicative (StateT s m) where
    pure a = StateT $ \s -> pure (a, s)
    (<*>) (StateT fma) (StateT ma) = StateT $
            \v -> do
                t1 <- fma v
                t2 <- ma (snd t1)
                pure ((fst t1) (fst t2), snd t2)

instance (Monad m) => Monad (StateT s m) where
    (>>=) (StateT ma) f = StateT $
            \v -> ma v >>= \(a, s) -> runStateT (f a) s

instance MonadTrans (StateT s) where
    lift m = StateT $ \s -> fmap (\a ->  (a, s)) m

instance (MonadIO m) => MonadIO (StateT s m) where
    liftIO = lift . liftIO
---

embedded :: MaybeT (ExceptT String (ReaderT () IO)) Int
embedded = MaybeT . ExceptT . ReaderT $ \() -> (pure . pure . pure) 1

---

class MonadTrans t where
    lift :: (Monad m) => m a -> t m a

class (Monad m) => MonadIO m where
    liftIO :: IO a -> m a

---
rDec :: Num a => R.Reader a a
rDec = R.reader $ (\r -> r -1)

rShow :: Show a => ReaderT a Identity String
rShow = ReaderT $ Identity . show

rPrintAndInc :: (Num a, Show a) => R.ReaderT a IO a
rPrintAndInc = R.ReaderT $ \a ->
    (putStrLn $ "Hi: " <> show a) >> (pure $ a - 1)

rPrintAndInc' :: (Num a, Show a) => R.ReaderT a IO a
rPrintAndInc' = R.ReaderT $ \a -> do
    putStrLn $ "Hi: " <> show a
    pure $ a - 1

sPrintIncAccum :: (Num a, Show a) => StateT a IO String
sPrintIncAccum = StateT $ \s ->
    (putStrLn $ "Hi: " <> show s) >> (pure (show s, s - 1))

---

isValid :: String -> Bool
isValid v = '!' `elem` v

maybeExcite :: MaybeT IO String
maybeExcite = MaybeT $ do
    v <- getLine
    pure $ if isValid v
            then Just v
            else Nothing
    -- guard $ isValid v
    -- return v

doExcite :: IO ()
doExcite = do
    putStrLn "say something excite!"
    excite <- runMaybeT maybeExcite
    case excite of
        Nothing -> putStrLn "MOAR EXCITE"
        Just e  -> putStrLn $ "Good, was very excite: " ++ e

