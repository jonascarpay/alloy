{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Lib.Bound where

import Control.Monad
import Control.Monad.Trans
import Data.List.NonEmpty (NonEmpty (..))

data Var b a = Bound b | Free a
  deriving (Eq, Show, Functor, Foldable, Traversable)

instance Applicative (Var b) where
  pure = Free
  (<*>) = ap

instance Monad (Var b) where
  Free a >>= f = f a
  Bound b >>= _ = Bound b

newtype Scope b f a = Scope {unScope :: f (Var b (f a))}
  deriving (Functor, Foldable, Traversable)

instance Monad f => Applicative (Scope b f) where
  pure = Scope . pure . pure . pure
  (<*>) = ap

instance Monad f => Monad (Scope b f) where
  Scope sa >>= f =
    Scope $
      sa >>= \case
        Free a -> a >>= unScope . f
        Bound b -> pure $ Bound b

instance MonadTrans (Scope b) where
  lift = Scope . pure . Free

abstract :: forall b f a. Monad f => (a -> Maybe b) -> f a -> Scope b f a
abstract f m = Scope $ go <$> m
  where
    go :: a -> Var b (f a)
    go a = case f a of
      Just b -> Bound b
      Nothing -> Free (pure a)

abstract1 :: (Monad f, Eq a) => a -> f a -> Scope () f a
abstract1 a = abstract $ \v -> if a == v then Just () else Nothing

instantiate :: Monad f => (b -> f a) -> Scope b f a -> f a
instantiate f (Scope m) =
  m >>= \case
    Free a -> a
    Bound b -> f b

instantiate1 :: Monad f => f a -> Scope () f a -> f a
instantiate1 = instantiate . const

closed :: Traversable f => f a -> Either (NonEmpty a) (f b)
closed = traverse (Left . singleton)
  where
    singleton a = a :| []

isClosed :: Foldable f => f a -> Bool
isClosed = null

-- lam :: Eq a => a -> Exp a -> Exp a
-- lam arg = Lam . abstract1 arg

-- recAttr :: Ord a => [(a, Exp a)] -> Map a (Scope a Exp a)
-- recAttr binds = abstract (\a -> if M.member a m then Just a else Nothing) <$> m
--   where
--     m = M.fromList binds

-- instantiateAttr :: forall a b. Ord a => Map a (Scope a Exp b) -> a -> Maybe (Exp b)
-- instantiateAttr m a = instantiate unsafe <$> M.lookup a m
--   where
--     unsafe :: a -> Exp b
--     unsafe = instantiate unsafe . fromJust . flip M.lookup m
