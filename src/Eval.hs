{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Expr
import Prettyprinter

type ThunkID = Int

--TODO Closure Args (p -> m r)
data ValueF val
  = VInt Int
  | VClosure Name Expr (Map Name ThunkID)
  | VAttr (Map Name val)
  deriving (Functor, Foldable, Traversable)

prettyVal :: Value -> Doc ann
prettyVal (Fix (VInt n)) = pretty n
prettyVal (Fix (VAttr attrs)) = braces $ align . vcat $ (\(a, x) -> hsep [pretty a, "=", prettyVal x <> ";"]) <$> M.toList attrs
prettyVal (Fix VClosure {}) = "<<closure>>"

type Value = Fix ValueF

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
    step (Attr m) = VAttr <$> traverse defer m
    step (ASTLit (BlockF _)) = throwError "I don't know what to do with code literals yet"

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
