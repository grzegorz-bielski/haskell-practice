module FromChapter26 where

newtype MaybeT m a = MaybeT { runMaybeT :: m (Maybe a)}

instance (Functor m) => Functor (MaybeT m) where
     -- apply f on value inside Maybe, lifting over `m` and Maybe
    fmap f (MaybeT ma) = MaybeT $ (fmap . fmap) f ma


instance (Applicative m) => Applicative (MaybeT m) where
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

newtype EitherT e m a = EitherT { runEitherT :: m (Either e a) }


