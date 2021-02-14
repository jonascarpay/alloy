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
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Debug.Trace
import Expr
import Lens.Micro.Platform
import Program

type ThunkID = Int

--TODO Closure Args (p -> m r)
--TODO How to properly handle RT Vars
--TODO I think spliced variables can escape their scope?
-- maybe just tag them from fresh to make sure
data ValueF val
  = VInt Int
  | VClosure Name Expr Env
  | VAttr (Map Name val)
  | VRTVar Name
  | VBlock (Closure (Block RTExpr))
  | VList (Seq val)
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

newtype LazyT m a = LazyT {_unLazyT :: RWST Env () (Int, Map ThunkID Thunk) (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadState (Int, Map ThunkID Thunk))

type Lazy = LazyT Identity

type Env = Map Name (Either ThunkID ())

lookupVar :: Name -> (ThunkID -> Lazy r) -> (() -> Lazy r) -> Lazy r
lookupVar name kct krt = do
  asks (M.lookup name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (Left tid) -> kct tid
    Just (Right _) -> krt ()

bindThunk :: Name -> ThunkID -> Env -> Env
bindThunk n tid = M.insert n (Left tid)

bindRtvar :: Name -> () -> Env -> Env
bindRtvar n _ = M.insert n (Right ())

yCombinator :: Expr
yCombinator = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

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
  local (bindThunk "builtins" tBuiltins) m

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
    (VClosure arg body env) -> local (const $ bindThunk arg tx env) (step body)
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (const . pure $ VRTVar x)
step (Lam arg body) = VClosure arg body <$> ask
step (Arith op a b) = do
  step a >>= \case
    VInt va ->
      step b >>= \case
        VInt vb -> pure (VInt $ arith op va vb)
        _ -> throwError "Arithmetic on a non-integer"
    _ -> throwError "Arithmetic on a non-integer"
step (Attr m) = VAttr <$> traverse deferExpr m
step (Acc f em) =
  step em >>= \case
    VAttr m -> do
      tid <- M.lookup f m <?> ("Accessing unknown field " <> f)
      force tid
    _ -> throwError "Accessing field of not an attribute set"
step (BlockExpr b) = VBlock . Closure mempty <$> genBlock b
step (List l) = VList <$> traverse deferExpr l

genBlock :: Block Expr -> Lazy (Block RTExpr)
genBlock (Block stmts) = Block <$> genStmt stmts

genStmt :: [Stmt Expr] -> Lazy [Stmt RTExpr]
genStmt [Break expr] = pure . Break <$> rtFromExpr expr
genStmt (Break _ : _) = throwError "Break is not final expression in block"
genStmt (Decl name expr : r) = do
  expr' <- rtFromExpr expr
  r' <- local (bindRtvar name ()) (genStmt r)
  pure (Decl name expr' : r')
genStmt (Assign name expr : r) = do
  name' <-
    let ct tid =
          force tid >>= \case
            VRTVar v -> pure v
            _ -> throwError $ "Assigning to non-runtime-variable " <> show name
        rt _ = pure name
     in lookupVar name ct rt
  expr' <- rtFromExpr expr
  (Assign name' expr' :) <$> genStmt r
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  (ExprStmt expr' :) <$> genStmt r
genStmt [] = pure []

-- TODO document why this is split into rtFromExpr and rtFromVal
-- TODO see if there is potential unification
-- TODO really this is just for reusing arithmetic ops (and vars?) I think which may be unnecessary
rtFromExpr :: Expr -> Lazy RTExpr
rtFromExpr (Arith op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  pure $ RTArith op rta rtb
rtFromExpr (Var n) = lookupVar n (deepEval >=> rtFromVal) (const $ pure $ RTVar n)
rtFromExpr expr@App {} = deepEvalExpr expr >>= rtFromVal
rtFromExpr expr@Lam {} = deepEvalExpr expr >>= rtFromVal
rtFromExpr expr@Lit {} = deepEvalExpr expr >>= rtFromVal
rtFromExpr expr@Attr {} = deepEvalExpr expr >>= rtFromVal
rtFromExpr expr@Acc {} = deepEvalExpr expr >>= rtFromVal
rtFromExpr expr@BlockExpr {} = deepEvalExpr expr >>= rtFromVal

-- TODO move to rtFromExpr where clause to emphasize that it is not used elsewhere
-- TODO handle(tell) the closure form VBlock
rtFromVal :: Value -> Lazy RTExpr
rtFromVal (Fix (VInt n)) = pure $ RTLit n
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VAttr {}) = throwError "can't handle attribute set values yet"
rtFromVal (Fix (VBlock (Closure _USEME b))) = pure $ RTBlock b
rtFromVal (Fix (VRTVar n)) = pure $ RTVar n

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
