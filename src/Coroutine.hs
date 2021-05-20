{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE UndecidableInstances #-}

module Coroutine where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.Writer
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

-- | Run a coroutine until completion, retrying every time it suspends.
-- Note that if a computation infinitely stalls (e.g. 'fix suspend') this will cause infinite recursion.
runCoroutine :: Monad m => Coroutine m r -> m r
runCoroutine (Coroutine m) = m >>= either runCoroutine pure

{-# INLINE liftP2 #-}
liftP2 :: (Functor m, MonadCoroutine m) => (a -> b -> c) -> m a -> m b -> m c
liftP2 f a = par (f <$> a)

{-# INLINE liftP3 #-}
liftP3 :: (Functor m, MonadCoroutine m) => (a -> b -> c -> d) -> m a -> m b -> m c -> m d
liftP3 f a b = par (liftP2 f a b)

{-# INLINE liftP4 #-}
liftP4 :: (Functor m, MonadCoroutine m) => (a -> b -> c -> d -> e) -> m a -> m b -> m c -> m d -> m e
liftP4 f a b c = par (liftP3 f a b c)

instance Applicative m => MonadCoroutine (Coroutine m) where
  par mf ma =
    Coroutine $
      let go (Right f) (Right a) = Right (f a)
          go (Right f) (Left ka) = Left (f <$> ka)
          go (Left kf) (Right a) = Left $ ($ a) <$> kf
          go (Left kf) (Left ka) = Left $ par kf ka
       in go <$> resume mf <*> resume ma
  suspend = Coroutine . pure . Left

instance (Semigroup w, Functor m, MonadCoroutine m) => MonadCoroutine (WriterT w m) where
  suspend (WriterT m) = WriterT $ suspend m
  par (WriterT mf) (WriterT ma) = WriterT $ par ((\(f, w) (a, w') -> (f a, w <> w')) <$> mf) ma

instance (MonadCoroutine m) => MonadCoroutine (ReaderT r m) where
  suspend (ReaderT m) = ReaderT $ suspend . m
  par (ReaderT mf) (ReaderT ma) = ReaderT $ \r -> par (mf r) (ma r)
