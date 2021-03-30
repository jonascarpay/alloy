{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Coroutine where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Bifunctor

-- | Monad transformer that allows a computation to be suspended.
-- The goal is that once we hit a point where not enough type information is available about a particular variable, we suspend the computation and return to it at a later point.
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

class MonadCoroutine m where
  -- | Run two computations until suspension.
  -- If either suspends, this will produce a suspended computation.
  -- The difference with normal (<*>) and (>>=) is that here, if the first computation suspends, it will still attempt to run the second computation.
  par :: m (a -> b) -> m a -> m b

  suspend :: m a -> m a

runCoroutine :: Monad m => Coroutine m r -> m r
runCoroutine (Coroutine m) = m >>= either runCoroutine pure

liftP2 :: (Functor m, MonadCoroutine m) => (a -> b -> c) -> m a -> m b -> m c
liftP2 f a = par (f <$> a)

liftP3 :: (Functor m, MonadCoroutine m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftP3 f a b = par (liftP2 f a b)

instance Applicative m => MonadCoroutine (Coroutine m) where
  par mf ma =
    Coroutine $
      let go (Right f) (Right a) = Right (f a)
          go (Right f) (Left ka) = Left (f <$> ka)
          go (Left kf) (Right a) = Left $ ($ a) <$> kf
          go (Left kf) (Left ka) = Left $ par kf ka
       in go <$> resume mf <*> resume ma
  suspend = Coroutine . pure . Left
