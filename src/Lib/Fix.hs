{-# LANGUAGE DeriveTraversable #-}

module Lib.Fix where

import Control.Monad

newtype Fix f = Fix (f (Fix f))

{--
-- TODO
-- none of these are actually used
newtype Comp f g a = Comp (f (g a))

convert :: Functor f => Fix (Comp (Either a) f) -> Free f a
convert (Fix (Comp (Left a))) = Pure a
convert (Fix (Comp (Right f))) = Free (convert <$> f)

data Free f a
  = Pure a
  | Free (f (Free f a))
  deriving (Functor)

instance Functor f => Applicative (Free f) where
  pure = Pure
  (<*>) = ap

instance Functor f => Monad (Free f) where
  Pure a >>= fn = fn a
  Free f >>= fn = Free $ (>>= fn) <$> f
--}
