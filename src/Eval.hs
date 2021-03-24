{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TemplateHaskell #-}

module Eval where

import Control.Monad.Except
import Control.Monad.RWS as RWS
import Control.Monad.Writer
import Data.Foldable
import Data.Functor.Identity
import Data.Hashable
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe
import Data.Sequence (Seq)
import Data.Void
import Expr
import Lens.Micro.Platform
import Program
import Typecheck

type ThunkID = Int

data ValueF val
  = VPrim Prim
  | VClosure Name Expr Context -- TODO benchmark if we can safely remove this
  | VClosure' (ThunkID -> Eval (ValueF ThunkID))
  | VType Type
  | VAttr (Map Name val)
  | VRTVar Name
  | VBlockLabel Name
  | VSelf Int
  | VBlock Dependencies (RTBlock PreCall (Maybe Type))
  | VFunc Dependencies (Either TempID GUID)
  | VList (Seq val)
  deriving (Functor, Foldable, Traversable)

arith :: Num n => ArithOp -> n -> n -> n
arith Add = (+)
arith Sub = (-)
arith Mul = (*)

comp :: Ord n => CompOp -> n -> n -> Bool
comp Eq = (==)
comp Neq = (/=)
comp Lt = (<)
comp Gt = (>)
comp Leq = (<=)
comp Geq = (>=)

type Value = Fix ValueF

data ThunkF m v = Deferred (m v) | Computed v

type Thunk = ThunkF Eval (ValueF ThunkID)

newtype EvalT m a = EvalT
  { _unLazyT ::
      RWST
        EvalEnv
        Dependencies
        EvalState
        (ExceptT String m)
        a
  }
  deriving (Functor, Applicative, Monad, MonadReader EvalEnv, MonadError String, MonadState EvalState, MonadWriter Dependencies)

data EvalState = EvalState
  { _thunkSource :: Int,
    _thunks :: Map ThunkID Thunk,
    _tempSource :: Int
  }

data Binding
  = BThunk ThunkID
  | BRTVar
  | BBlockLabel
  | BSelf Int
  deriving (Eq, Show)

isThunk :: Binding -> Bool
isThunk (BThunk _) = True
isThunk _ = False

type Eval = EvalT Identity

type Env = Map Name Binding

data Context = Context
  { _ctxBinds :: Map Name Binding,
    _ctxName :: Maybe Name
  }
  deriving (Eq, Show)

data EvalEnv = EvalEnv
  { _ctx :: Context,
    _envFnDepth :: Int,
    _envFnStack :: [([Type], Type)]
  }
  deriving (Eq, Show)

makeLenses ''Context
makeLenses ''EvalEnv
makeLenses ''EvalState

withName :: Monad m => Name -> EvalT m a -> EvalT m a
withName name = local (ctx . ctxName ?~ name)

lookupVar ::
  (MonadReader EvalEnv m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  m r ->
  m r ->
  (Int -> m r) ->
  m r
lookupVar name kct krt klbl kself = do
  view (ctx . ctxBinds . at name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (BThunk tid) -> kct tid
    Just BRTVar -> krt
    Just BBlockLabel -> klbl
    Just (BSelf n) -> kself n

bindThunk :: Name -> ThunkID -> Context -> Context
bindThunk name tid = ctxBinds . at name ?~ BThunk tid

bindThunks :: [(Name, ThunkID)] -> Context -> Context
bindThunks = appEndo . mconcat . fmap (Endo . uncurry bindThunk)

bindRtvar :: Name -> Env -> Env
bindRtvar n = M.insert n BRTVar

runEval :: Eval a -> Either String a
runEval (EvalT m) = fmap fst $ runExcept $ evalRWST m (EvalEnv (Context mempty Nothing) 0 mempty) (EvalState 0 mempty 0)

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal . VPrim $ PInt 9
  tStruct <- deferVal $ VClosure' (force >=> struct)
  tTypes <-
    deferAttrs
      [ ("int", VType TInt),
        ("double", VType TDouble),
        ("void", VType TVoid),
        ("bool", VType TBool)
      ]
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("types", tTypes),
          ("struct", tStruct)
        ]
  local (ctx %~ bindThunk "builtins" tBuiltins) m

struct :: ValueF ThunkID -> Eval (ValueF ThunkID)
struct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
struct _ = throwError "Con/struct/ing a struct from s'thing other than an attr set"

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

localState :: MonadState s m => (s -> s) -> m a -> m a
localState f m = do
  s <- get
  put (f s)
  a <- m
  put s
  pure a

freshTempId :: Eval TempID
freshTempId = state (\(EvalState us un ts) -> (ts, EvalState us un (ts + 1)))

functionBodyEnv :: [(Name, Type)] -> Type -> EvalEnv -> EvalEnv
functionBodyEnv typedArgs ret (EvalEnv (Context binds name) depth stack) =
  EvalEnv (Context binds' name) depth' stack'
  where
    (names, types) = unzip typedArgs
    depth' = depth + 1
    stack' = (types, ret) : stack
    binds' =
      flip (foldr bindRtvar) names
        . (at "self" ?~ BSelf depth)
        . M.filter isThunk
        $ binds

type RTEval = WriterT Dependencies Eval -- TODO rename this

resolveToRuntimeVar :: Name -> RTEval Name
resolveToRuntimeVar name = lift $ lookupVar name kComp (pure name) err (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to runtime variable"
    kComp tid =
      force tid >>= \case
        VRTVar v -> pure v
        _ -> err

resolveToBlockLabel :: Name -> RTEval Name
resolveToBlockLabel name = lift $ lookupVar name kComp err (pure name) (const err)
  where
    err = throwError $ "Variable " <> show name <> " did not resolve to block label"
    kComp tid =
      force tid >>= \case
        VBlockLabel v -> pure v
        _ -> err

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(EvalState n m t) -> (n, EvalState (n + 1) (M.insert n thunk m) t)

deferM :: Eval (ValueF ThunkID) -> Eval ThunkID
deferM = mkThunk . Deferred

deferVal :: ValueF ThunkID -> Eval ThunkID
deferVal = mkThunk . Computed

force :: ThunkID -> Eval (ValueF ThunkID)
force tid =
  use (thunks . at tid) >>= \case
    Just (Deferred m) -> do
      thunks . at tid .= Just (Deferred $ throwError "Infinite recursion")
      v <- m
      thunks . at tid .= Just (Computed v)
      pure v
    Just (Computed x) -> pure x
    Nothing -> throwError "Looking up invalid thunk?"
