{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}

module Eval (ValueF (..), Value, eval) where

import Control.Monad.Except
import Control.Monad.RWS
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Expr
import Lens.Micro.Platform
import Program

type ThunkID = Int

--TODO Closure Args (p -> m r)
data ValueF val
  = VInt Int
  | VClosure Name Expr Env
  | VAttr (Map Name val)
  | VBlock (Closure (Block RTExpr))
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

newtype LazyT m a = LazyT {_unLazyT :: RWST Env () (Int, Map ThunkID Thunk) (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadState (Int, Map ThunkID Thunk))

type Lazy = LazyT Identity

yCombinator :: Expr
yCombinator = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

(?>) :: MonadError e m => m (Maybe a) -> e -> m a
(?>) m e = m >>= maybe (throwError e) pure

(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) m e = maybe (throwError e) pure m

eval :: Expr -> Either String Value
eval eRoot = runLazy $ withBuiltins $ deepEvalExpr eRoot

runLazy :: Lazy a -> Either String a
runLazy (LazyT m) = fmap fst $ runExcept $ evalRWST m mempty (0, mempty)

withBuiltins :: Lazy a -> Lazy a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal $ VInt 9
  tFix <- deferExpr yCombinator
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("fix", tFix)
        ]
  local (M.insert "builtins" tBuiltins) m

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Lazy Value
deepEvalExpr = deferExpr >=> deepEval

deepEval :: ThunkID -> Lazy Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

step :: Expr -> Lazy (ValueF ThunkID)
step (Lit n) = pure $ VInt n
step (App f x) = do
  tx <- deferExpr x
  step f >>= \case
    (VClosure arg body env) -> local (const $ M.insert arg tx env) (step body)
    _ -> throwError "Calling a non-function"
step (Var x) = do
  tid <- asks (M.lookup x) ?> ("Unbound variable: " <> x)
  force tid
step (Lam arg body) = VClosure arg body <$> ask
step (Arith op a b) = do
  step a >>= \case
    VInt va ->
      step b >>= \case
        VInt vb -> pure (VInt $ arith op va vb)
        _ -> throwError "Adding a non-integer"
    _ -> throwError "Adding a non-integer"
step (Attr m) = VAttr <$> traverse deferExpr m
step (Acc f em) =
  step em >>= \case
    VAttr m -> do
      tid <- M.lookup f m <?> ("Accessing unknown field " <> f)
      force tid
    _ -> throwError "Accessing field of not an attribute set"
step (BlockExpr (Block _)) = throwError "Can't handle code blocks yet" -- pure $ VBlock (Closure mempty undefined)

-- TODO use the writer to track closed functinos
-- TODO what do runtime variables actually map to?
-- TODO rename this to something like runtimegen
-- TODO can the map be a reader?
-- does the count need to be a state?
newtype CodegenT m a = CodegenT
  { _unCodegenT ::
      RWST (Map Name Int) () Int m a
  }
  deriving (Functor, Applicative, Monad, MonadTrans, MonadState Int, MonadReader (Map Name Int))

-- TODO this forces undecidable instances, monomoprhize at some point
deriving instance MonadError e m => MonadError e (CodegenT m)

fresh :: Monad m => CodegenT m Int
fresh = state $ \n -> (n, n + 1)

type Codegen = CodegenT Lazy

runCodegen :: Codegen a -> Lazy a
runCodegen (CodegenT m) = fst <$> evalRWST m mempty 0

genBlock :: Block Expr -> Codegen (Block RTExpr)
genBlock = undefined

-- TODO document why this is split into rtFromExpr and rtFromVal
-- TODO see if there is potential unification
-- TODO really this is just for reusing arithmetic ops (and vars?) I think which may be unnecessary
rtFromExpr :: Expr -> Codegen RTExpr
rtFromExpr (Arith op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  pure $ RTArith op rta rtb
rtFromExpr (Var n) =
  asks (M.lookup n) >>= \case
    Nothing ->
      -- TODO lift only once?
      lift (asks $ M.lookup n) >>= \case
        Nothing -> throwError $ "Neither runtime nor comptime variable: " <> n
        Just tid -> lift (deepEval tid) >>= rtFromVal
    Just _ -> pure $ RTVar n
rtFromExpr expr@App {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lit {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal

-- TODO move to rtFromExpr where clause to emphasize that it is not used elsewhere
-- TODO handle(tell) the closure form VBlock
rtFromVal :: Value -> Codegen RTExpr
rtFromVal (Fix (VInt n)) = pure $ RTLit n
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VAttr {}) = throwError "can't handle attribute set values yet"
rtFromVal (Fix (VBlock (Closure _USEME b))) = pure $ RTBlock b

-- rtFromVal (Fix (VBlock _)) = throwError "can't handle attribute set values yet"

mkThunk :: Thunk -> Lazy ThunkID
mkThunk thunk = state $ \(n, m) -> (n, (n + 1, M.insert n thunk m))

deferM :: Lazy (ValueF ThunkID) -> Lazy ThunkID
deferM = mkThunk . Deferred

deferVal :: ValueF ThunkID -> Lazy ThunkID
deferVal = mkThunk . Computed

deferExpr :: Expr -> Lazy ThunkID
deferExpr expr = ask >>= \env -> mkThunk . Deferred $ local (const env) (step expr)

force :: ThunkID -> Lazy (ValueF ThunkID)
force tid =
  gets (M.lookup tid . snd) >>= \case
    Just (Deferred m) -> do
      v <- m
      _2 . at tid .= Just (Computed v)
      pure v
    Just (Computed x) -> pure x
    Nothing -> throwError "Looking up invalid thunk?"
