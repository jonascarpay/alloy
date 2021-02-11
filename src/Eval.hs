{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Expr
import Lens.Micro.Platform

type ThunkID = Int

--TODO Closure Args (p -> m r)
data ValueF val
  = VInt Int
  | VClosure Name Expr Env
  | VAttr (Map Name val)
  deriving (Functor, Foldable, Traversable)

arith :: ArithOp -> Int -> Int -> Int
arith Add = (+)
arith Sub = (-)
arith Mul = (*)

type Value = Fix ValueF

type Env = Map Name ThunkID

-- TODO Thunk m v = Deferred (m v) | Computed v
-- Thoughts on thunks:
-- it's scary how few problems not having the Env here actually caused.
-- not sure to what degree that's solved by the above
-- this seems memory intensive though?
data Thunk = Deferred Env Expr | Computed (ValueF ThunkID)

type Lazy = RWST Env () (Int, Map ThunkID Thunk) (Either String)

(?>) :: MonadError e m => m (Maybe a) -> e -> m a
(?>) m e = m >>= maybe (throwError e) pure

(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) m e = maybe (throwError e) pure m

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
    step (Var x) = do
      tid <- asks (M.lookup x) ?> ("Unbound variable: " <> x)
      force tid
    step (Lam arg body) = VClosure arg body <$> ask
    step (Arith op (Fix a) (Fix b)) = do
      step a >>= \case
        VInt va ->
          step b >>= \case
            VInt vb -> pure (VInt $ arith op va vb)
            _ -> throwError "Adding a non-integer"
        _ -> throwError "Adding a non-integer"
    step (Attr m) = VAttr <$> traverse defer m
    step (Acc f (Fix em)) =
      step em >>= \case
        VAttr m -> do
          tid <- M.lookup f m <?> ("Accessing unknown field " <> f)
          force tid
        _ -> throwError "Accessing field of not an attribute set"
    step (ASTLit _) = throwError "I don't know what to do with code literals yet"

    defer :: Expr -> Lazy ThunkID
    defer expr = ask >>= \env -> state $ \(n, m) -> (n, (n + 1, M.insert n (Deferred env expr) m))

    force :: ThunkID -> Lazy (ValueF ThunkID)
    force tid =
      gets (M.lookup tid . snd) >>= \case
        Just (Deferred env (Fix expr)) -> local (const env) $ do
          v <- step expr
          _2 . at tid .= Just (Computed v)
          pure v
        Just (Computed x) -> pure x
        Nothing -> throwError "Looking up invalid thunk?"
