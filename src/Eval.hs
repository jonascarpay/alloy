{-# LANGUAGE LambdaCase #-}

module Eval where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Expr

type ThunkID = Int

-- TODO Thunk m v = Deferred (m v) | Computed v
data Thunk = Deferred Expr | Computed (ValueF ThunkID)

type Lazy = RWST (Map Name ThunkID) () (Int, Map ThunkID Thunk) (Either String)

eval :: Expr -> Either String Value
eval (Fix eRoot) = runLazy $ step eRoot >>= deepEval
  where
    runLazy :: Lazy a -> Either String a
    runLazy m = fst <$> evalRWST m mempty (0, mempty)
    deepEval :: ValueF ThunkID -> Lazy Value
    deepEval v = Fix <$> traverse (force >=> deepEval) v
    step :: ExprF Expr -> Lazy (ValueF ThunkID)
    step (Lit n) = pure $ VInt n
    step (App (Fix f) x) = do
      tx <- defer x
      step f >>= \case
        (VClosure arg (Fix body) env) -> local (const $ M.insert arg tx env) (step body)
        _ -> throwError "Calling a non-function"
    step (Var x) =
      asks (M.lookup x) >>= \case
        Nothing -> throwError "Unbound variable"
        Just tid -> force tid
    step (Lam arg body) = VClosure arg body <$> ask
    step (Add (Fix a) (Fix b)) = do
      step a >>= \case
        VInt va ->
          step b >>= \case
            VInt vb -> pure (VInt (va + vb))
            _ -> throwError "Adding a non-integer"
        _ -> throwError "Adding a non-integer"
    defer :: Expr -> Lazy ThunkID
    defer expr = state $ \(n, m) -> (n, (n + 1, M.insert n (Deferred expr) m))
    force :: ThunkID -> Lazy (ValueF ThunkID)
    force tid =
      gets (M.lookup tid . snd) >>= \case
        Just (Deferred (Fix expr)) -> do
          v <- step expr
          _2 . at tid .= Just (Computed v)
          pure v
        Just (Computed x) -> pure x
        Nothing -> throwError "Looking up invalid thunk?"
