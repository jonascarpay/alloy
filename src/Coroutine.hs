{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Coroutine where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Bifunctor

newtype Coroutine m r = Coroutine {resume :: m (Either (Coroutine m r) r)}

instance Functor m => Functor (Coroutine m) where
  fmap f (Coroutine m) = Coroutine $ bimap (fmap f) f <$> m

instance Monad m => Applicative (Coroutine m) where
  pure = Coroutine . pure . pure
  (<*>) = ap

instance Monad m => Monad (Coroutine m) where
  Coroutine m >>= f =
    Coroutine $
      m >>= \case
        Left k -> pure $ Left (k >>= f)
        Right r -> resume (f r)

instance MonadTrans Coroutine where lift = Coroutine . fmap Right

instance MonadIO m => MonadIO (Coroutine m) where liftIO = lift . liftIO

instance MonadReader r m => MonadReader r (Coroutine m) where
  local f (Coroutine m) = Coroutine $ local f m
  ask = lift ask

instance MonadState s m => MonadState s (Coroutine m) where
  state = lift . state

instance MonadError e m => MonadError e (Coroutine m) where
  throwError = lift . throwError
  catchError (Coroutine m) catch = Coroutine $ catchError m (resume . catch)

suspend :: Applicative m => Coroutine m r -> Coroutine m r
suspend = Coroutine . pure . Left

runCoroutine :: Monad m => Coroutine m r -> m r
runCoroutine (Coroutine m) = m >>= either runCoroutine pure

liftP2 :: Monad m => (a -> b -> c) -> Coroutine m a -> Coroutine m b -> Coroutine m c
liftP2 f a = par (f <$> a)

liftP3 :: Monad m => (a -> b -> c -> d) -> Coroutine m a -> Coroutine m b -> Coroutine m c -> Coroutine m d
liftP3 f a b = par (liftP2 f a b)

-- | Run two computations until suspension.
-- If either suspends, this will produce a suspended computation.
-- The difference with normal (<*>) and (>>=) is that here, if the first computation suspends, it will still attempt to run the second computation.
par :: Monad m => Coroutine m (a -> b) -> Coroutine m a -> Coroutine m b
par mf ma =
  Coroutine $
    let go (Right f) (Right a) = Right (f a)
        go (Right f) (Left ka) = Left (f <$> ka)
        go (Left kf) (Right a) = Left $ ($ a) <$> kf
        go (Left kf) (Left ka) = Left $ par kf ka
     in go <$> resume mf <*> resume ma
