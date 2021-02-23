{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}

module Eval (ValueF (..), Value, eval, RuntimeEnv (..)) where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Writer
import Data.Either (isLeft)
import Data.Foldable
import Data.Functor.Identity
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence (Seq)
import Expr
import Lens.Micro.Platform
import Program
import Typecheck

type ThunkID = Int

--TODO Closure Args (p -> m r)
--TODO How to properly handle RT Vars
--TODO I think spliced variables can escape their scope?
-- maybe just tag them from fresh to make sure
data ValueF val
  = VPrim Prim
  | VClosure Name Expr Env -- TODO benchmark if we can safely remove this
  | VClosure' (ThunkID -> Eval (ValueF ThunkID))
  | VType Type
  | VAttr (Map Name val)
  | VRTVar Name
  | VBlock RuntimeEnv (RTBlock (Maybe Type)) -- TODO Move Type into Block?
  | VFunc RuntimeEnv Function -- TODO Type here could be a val?
  | VList (Seq val)
  deriving (Functor, Foldable, Traversable)

arith :: Num n => ArithOp -> n -> n -> n
arith Add = (+)
arith Sub = (-)
arith Mul = (*)

type Value = Fix ValueF

-- Thoughts on thunks:
-- it's scary how few problems not having the Env here actually caused.
-- not sure to what degree that's solved by the above
-- this seems memory intensive though?

data ThunkF m v = Deferred (m v) | Computed v

type Thunk = ThunkF Eval (ValueF ThunkID)

newtype EvalT m a = EvalT {_unLazyT :: RWST Env RuntimeEnv (Int, Map ThunkID Thunk) (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadState (Int, Map ThunkID Thunk), MonadWriter RuntimeEnv)

type Eval = EvalT Identity

type Env = Map Name (Either ThunkID ())

lookupVar ::
  (MonadReader Env m, MonadError String m) =>
  Name ->
  (ThunkID -> m r) ->
  (() -> m r) ->
  m r
lookupVar name kct krt = do
  asks (M.lookup name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (Left tid) -> kct tid
    Just (Right typ) -> krt typ

bindThunk :: Name -> ThunkID -> Env -> Env
bindThunk n tid = M.insert n (Left tid)

bindRtvar :: Name -> Env -> Env
bindRtvar n = M.insert n (Right ())

yCombinator :: Expr
yCombinator = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) m e = maybe (throwError e) pure m

eval :: Expr -> Either String Value
eval eRoot = runEval $ withBuiltins $ deepEvalExpr eRoot

runEval :: Eval a -> Either String a
runEval (EvalT m) = fmap fst $ runExcept $ evalRWST m mempty (0, mempty)

-- deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
-- deferAttrs attrs = do
--   attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
--   deferVal $ VAttr $ M.fromList attrs'

withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal . VPrim $ PInt 9
  tFix <- deferExpr yCombinator
  tDouble <- deferVal (VType TDouble)
  tInt <- deferVal (VType TInt)
  tVoid <- deferVal (VType TVoid)
  tStruct <- deferVal $ VClosure' (force >=> struct)
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("fix", tFix),
          ("double", tDouble),
          ("int", tInt),
          ("void", tVoid),
          ("struct", tStruct)
        ]
  local (bindThunk "builtins" tBuiltins) m

struct :: ValueF ThunkID -> Eval (ValueF ThunkID)
struct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
struct _ = throwError "Con/struct/ing a struct from s'thing other than an attr set"

-- TODO this technically creates an unnecessary thunk since we defer and then
-- immediately evaluate but I don't think we care
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

step :: Expr -> Eval (ValueF ThunkID)
step (Prim p) = pure $ VPrim p
step (App f x) = do
  step f >>= \case
    (VClosure arg body env) -> do
      tx <- deferExpr x
      local (const $ bindThunk arg tx env) (step body)
    (VClosure' m) -> deferExpr x >>= m -- FIXME this will eventually break, cannot handle proper closures
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (const $ pure $ VRTVar x)
step (Lam arg body) = VClosure arg body <$> ask
step (Let binds body) = do
  n0 <- gets fst
  let (names, exprs) = unzip binds
      env env0 = foldr (\(name, tid) m -> M.insert name (Left tid) m) env0 $ zip names [n0 ..]
  local env $ mapM_ deferExpr exprs >> step body
step (Arith op a b) = do
  va <- step a
  vb <- step b
  case (va, vb) of
    (VPrim (PInt pa), VPrim (PInt pb)) -> pure . VPrim . PInt $ arith op pa pb
    (VPrim (PDouble pa), VPrim (PDouble pb)) -> pure . VPrim . PDouble $ arith op pa pb
    _ -> throwError "Arithmetic on a non-integer"
step (Attr m) = VAttr <$> traverse deferExpr m
step (Acc f em) =
  step em >>= \case
    VAttr m -> do
      tid <- M.lookup f m <?> ("Accessing unknown field " <> f)
      force tid
    _ -> throwError "Accessing field of not an attribute set"
step (BlockExpr b) =
  uncurry VBlock <$> genBlock b
step (List l) = VList <$> traverse deferExpr l
step (Func args ret body) = do
  typedArgs <- (traverse . traverse) evalType args
  retType <- evalType ret
  local (flip (foldr bindRtvar) (fst <$> typedArgs) . forgetRT) $ do
    (env, blk) <- genBlock body
    either throwError (pure . VFunc env) $
      typecheckFunction env typedArgs retType blk

forgetRT :: Env -> Env
forgetRT = M.filter isLeft

evalType :: Expr -> Eval Type
evalType expr =
  step expr >>= \case
    VType tp -> pure tp
    _ -> throwError "Expected type, got not-a-type"

genBlock ::
  Block Expr Expr ->
  Eval (RuntimeEnv, Block (Maybe Type) (RTExpr (Maybe Type) (Maybe Type)))
genBlock (Block stmts) = do
  (stmts', env) <- runWriterT $ genStmt stmts
  pure (env, Block stmts')

type RTEval = WriterT RuntimeEnv Eval -- TODO rename this

tellFunction :: Function -> RTEval ()
tellFunction func = tell (RuntimeEnv $ M.singleton (fnGuid func) func)

-- TODO This processes statements as a list because a declaration is relevant in the tail of the list
-- but that's kinda hacky and might overlap with type checking
genStmt ::
  [Stmt Expr Expr] ->
  RTEval [Stmt (Maybe Type) (RTExpr (Maybe Type) (Maybe Type))]
genStmt (Return expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (Return expr' : r')
genStmt (Decl name typ expr : r) = do
  typ' <- lift $ evalType typ
  expr' <- rtFromExpr expr
  r' <- local (bindRtvar name) (genStmt r)
  pure (Decl name (Just typ') expr' : r')
genStmt (Assign name expr : r) = do
  name' <- do
    let ct tid =
          force tid >>= \case
            VRTVar v -> pure v
            _ -> throwError $ "Assigning to non-runtime-variable " <> show name
        rt _ = pure name
     in lift $ lookupVar name ct rt
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (Assign name' expr' : r')
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  r' <- genStmt r
  pure (ExprStmt expr' : r')
genStmt [] = pure []

-- TODO document why this is split into rtFromExpr and rtFromVal
-- TODO see if there is potential unification
-- TODO really this is just for reusing arithmetic ops (and vars?) I think which may be unnecessary
rtFromExpr ::
  Expr ->
  RTEval (RTExpr (Maybe Type) (Maybe Type))
rtFromExpr (Arith op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  pure $ RTArith op rta rtb Nothing
rtFromExpr (Var n) =
  lookupVar
    n
    (lift . deepEval >=> rtFromVal)
    (const $ pure (RTVar n Nothing))
rtFromExpr (App f x) = do
  tf <- lift $ deferExpr f
  lift (force tf) >>= \case
    VClosure arg body env -> do
      tx <- lift $ deferExpr x
      local (const $ bindThunk arg tx env) $
        lift (deepEvalExpr body) >>= rtFromVal
    VFunc childEnv func ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          tell childEnv
          tellFunction func
          pure $ RTCall (fnGuid func) rtArgs (Just $ fnRet func)
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    _ -> throwError "Calling a non-function"
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Prim {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Func {} = lift (deepEvalExpr expr) >>= rtFromVal

rtLitFromPrim :: Prim -> RTEval RTLiteral
rtLitFromPrim (PInt n) = pure $ RTInt n
rtLitFromPrim (PDouble n) = pure $ RTDouble n
rtLitFromPrim (PBool _) = throwError "runtime bools aren't a thing"

-- (VFunc arg body) ->
--   let name = "function_" <> show tf
--    in pure
-- TODO move to rtFromExpr where clause to emphasize that it is not used elsewhere
-- TODO handle(tell) the closure form VBlock
rtFromVal :: Value -> RTEval (RTExpr (Maybe Type) (Maybe Type))
rtFromVal (Fix (VPrim p)) = fmap (`RTLiteral` Nothing) (rtLitFromPrim p)
rtFromVal (Fix (VAttr m)) = do
  m' <- flip M.traverseWithKey m $ \key field ->
    rtFromVal field >>= \case
      RTLiteral l _ -> pure l
      _ -> throwError $ "Field " <> key <> " did not evaluate to a literal"
  pure $ RTLiteral (RTStruct m') Nothing
rtFromVal (Fix (VRTVar n)) = pure $ RTVar n Nothing
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VClosure' {}) = throwError "partially applied closure' in runtime expression"
rtFromVal (Fix VList {}) = throwError "can't handle list values yet"
rtFromVal (Fix VType {}) = throwError "can't handle type"
rtFromVal (Fix VFunc {}) = throwError "Function values don't make sense here"
rtFromVal (Fix (VBlock env b)) = RTBlock b Nothing <$ tell env

-- rtFromVal typ (Fix (VRTVar (t, n))) = RTVar n <$> unify typ (Just t)

-- rtFromVal (Fix (VBlock _)) = throwError "can't handle attribute set values yet"

mkThunk :: Thunk -> Eval ThunkID
mkThunk thunk = state $ \(n, m) -> (n, (n + 1, M.insert n thunk m))

deferM :: Eval (ValueF ThunkID) -> Eval ThunkID
deferM = mkThunk . Deferred

deferVal :: ValueF ThunkID -> Eval ThunkID
deferVal = mkThunk . Computed

deferExpr :: Expr -> Eval ThunkID
deferExpr expr = ask >>= \env -> mkThunk . Deferred $ local (const env) (step expr)

force :: ThunkID -> Eval (ValueF ThunkID)
force tid =
  gets (M.lookup tid . snd) >>= \case
    Just (Deferred m) -> do
      v <- m
      _2 . at tid .= Just (Computed v)
      pure v
    Just (Computed x) -> pure x
    Nothing -> throwError "Looking up invalid thunk?"
