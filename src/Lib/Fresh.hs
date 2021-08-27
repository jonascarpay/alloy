{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Lib.Fresh
  ( Fresh,
    runFresh,
    fresh,
  )
where

import Control.Monad.State
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Expr (Symbol)

newtype FreshT m a = FreshT {_unFresh :: StateT (Map Symbol Int) m a}
  deriving (Functor, Monad, Applicative)

type Fresh = FreshT Identity

fresh :: Monad m => Symbol -> FreshT m Symbol
fresh sym = FreshT $ state f
  where
    mkSym :: Symbol -> Int -> Symbol
    mkSym sym n = sym <> T.pack (show n)
    freshAt :: Map Symbol Int -> Symbol -> Int -> (Symbol, Map Symbol Int)
    freshAt m sym = go
      where
        go guess =
          let sym' = mkSym sym guess
           in if M.member sym' m
                then go (guess + 1)
                else (sym', M.insert sym' 0 m)
    f :: Map Symbol Int -> (Symbol, Map Symbol Int)
    f m = case M.lookup sym m of
      Nothing -> (sym, M.insert sym 0 m)
      Just n -> freshAt m sym n

runFreshT :: Monad m => FreshT m a -> m a
runFreshT (FreshT m) = evalStateT m mempty

runFresh :: Fresh a -> a
runFresh = runIdentity . runFreshT
