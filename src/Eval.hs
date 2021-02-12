{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval (ValueF (..), Value, eval) where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Functor.Identity
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

-- Thoughts on thunks:
-- it's scary how few problems not having the Env here actually caused.
-- not sure to what degree that's solved by the above
-- this seems memory intensive though?

data ThunkF m v = Deferred (m v) | Computed v

type Thunk = ThunkF Lazy (ValueF ThunkID)

type Env = Map Name ThunkID

newtype LazyT m a = LazyT {unLazyT :: RWST Env () (Int, Map ThunkID Thunk) (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadState (Int, Map ThunkID Thunk))

type Lazy = LazyT Identity

(?>) :: MonadError e m => m (Maybe a) -> e -> m a
(?>) m e = m >>= maybe (throwError e) pure

(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) m e = maybe (throwError e) pure m

eval :: Expr -> Either String Value
eval (Fix eRoot) = runLazy $ step eRoot >>= deepEval

runLazy :: Lazy a -> Either String a
runLazy m = fmap fst $ runExcept $ evalRWST (unLazyT $ withBuiltins m) mempty (0, mempty)

withBuiltins :: Lazy a -> Lazy a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal $ VInt 9
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine)
        ]
  local (M.insert "builtins" tBuiltins) m

deepEval :: ValueF ThunkID -> Lazy Value
deepEval v = Fix <$> traverse (force >=> deepEval) v

step :: ExprF Expr -> Lazy (ValueF ThunkID)
step (Lit n) = pure $ VInt n
step (App (Fix f) x) = do
  tx <- deferExpr x
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
step (Attr m) = VAttr <$> traverse deferExpr m
step (Acc f (Fix em)) =
  step em >>= \case
    VAttr m -> do
      tid <- M.lookup f m <?> ("Accessing unknown field " <> f)
      force tid
    _ -> throwError "Accessing field of not an attribute set"
step (ASTLit _) = throwError "I don't know what to do with code literals yet"

mkThunk :: Thunk -> Lazy ThunkID
mkThunk thunk = state $ \(n, m) -> (n, (n + 1, M.insert n thunk m))

deferM :: Lazy (ValueF ThunkID) -> Lazy ThunkID
deferM = mkThunk . Deferred

deferVal :: ValueF ThunkID -> Lazy ThunkID
deferVal = mkThunk . Computed

deferExpr :: Expr -> Lazy ThunkID
deferExpr (Fix expr) = ask >>= \env -> mkThunk . Deferred $ local (const env) (step expr)

force :: ThunkID -> Lazy (ValueF ThunkID)
force tid =
  gets (M.lookup tid . snd) >>= \case
    Just (Deferred m) -> do
      v <- m
      _2 . at tid .= Just (Computed v)
      pure v
    Just (Computed x) -> pure x
    Nothing -> throwError "Looking up invalid thunk?"
