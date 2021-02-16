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
  | VBlock RuntimeEnv (Block RTExpr)
  | VFunc RuntimeEnv [Name] (Block RTExpr)
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

type Thunk = ThunkF Eval (ValueF ThunkID)

newtype EvalT m a = EvalT {_unLazyT :: RWST Env RuntimeEnv (Int, Map ThunkID Thunk) (ExceptT String m) a}
  deriving (Functor, Applicative, Monad, MonadReader Env, MonadError String, MonadState (Int, Map ThunkID Thunk), MonadWriter RuntimeEnv)

type Eval = EvalT Identity

type Env = Map Name (Either ThunkID ())

newtype RuntimeEnv = RuntimeEnv {rtFunctions :: Map Name ([Name], Block RTExpr)}
  deriving (Eq, Show)

instance Semigroup RuntimeEnv where RuntimeEnv fns <> RuntimeEnv fns' = RuntimeEnv (fns <> fns')

instance Monoid RuntimeEnv where mempty = RuntimeEnv mempty

lookupVar :: (MonadReader Env m, MonadError String m) => Name -> (ThunkID -> m r) -> (() -> m r) -> m r
lookupVar name kct krt = do
  asks (M.lookup name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (Left tid) -> kct tid
    Just (Right _) -> krt ()

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

withBuiltins :: Eval a -> Eval a
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
deepEvalExpr :: Expr -> Eval Value
deepEvalExpr = deferExpr >=> deepEval

deepEval :: ThunkID -> Eval Value
deepEval tid = Fix <$> (force tid >>= traverse deepEval)

step :: Expr -> Eval (ValueF ThunkID)
step (Lit n) = pure $ VInt n
step (App f x) = do
  step f >>= \case
    (VClosure arg body env) -> do
      tx <- deferExpr x
      local (const $ bindThunk arg tx env) (step body)
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (const . pure $ VRTVar x)
step (Lam arg body) = VClosure arg body <$> ask
step (Let binds body) = do
  n0 <- gets fst
  let (names, exprs) = unzip binds
      env env0 = foldr (\(name, tid) m -> M.insert name (Left tid) m) env0 $ zip names [n0 ..]
  local env $ mapM_ deferExpr exprs >> step body
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
step (BlockExpr b) = uncurry VBlock <$> genBlock b
step (List l) = VList <$> traverse deferExpr l
step (Func args body) = local (flip (foldr bindRtvar) args . forgetRT) $ do
  step body >>= \case
    VBlock env b -> pure $ VFunc env args b
    _ -> throwError "Function body did not evaluate to code block"

forgetRT :: Env -> Env
forgetRT = M.filter isLeft

genBlock :: Block Expr -> Eval (RuntimeEnv, Block RTExpr)
genBlock (Block stmts) = (\(rtStmts, env) -> (env, Block rtStmts)) <$> runWriterT (genStmt stmts)

type RTEval = WriterT RuntimeEnv Eval -- TODO rename this

tellFunction :: Name -> [Name] -> Block RTExpr -> RTEval ()
tellFunction name args body = tell (RuntimeEnv $ M.singleton name (args, body))

genStmt :: [Stmt Expr] -> RTEval [Stmt RTExpr]
genStmt [Break expr] = pure . Break <$> rtFromExpr expr
genStmt (Break _ : _) = throwError "Break is not final expression in block"
genStmt (Decl name expr : r) = do
  expr' <- rtFromExpr expr
  r' <- local (bindRtvar name) (genStmt r)
  pure (Decl name expr' : r')
genStmt (Assign name expr : r) = do
  name' <-
    let ct tid =
          force tid >>= \case
            VRTVar v -> pure v
            _ -> throwError $ "Assigning to non-runtime-variable " <> show name
        rt _ = pure name
     in lift $ lookupVar name ct rt
  expr' <- rtFromExpr expr
  (Assign name' expr' :) <$> genStmt r
genStmt (ExprStmt expr : r) = do
  expr' <- rtFromExpr expr
  (ExprStmt expr' :) <$> genStmt r
genStmt [] = pure []

-- TODO document why this is split into rtFromExpr and rtFromVal
-- TODO see if there is potential unification
-- TODO really this is just for reusing arithmetic ops (and vars?) I think which may be unnecessary
rtFromExpr :: Expr -> RTEval RTExpr
rtFromExpr (Arith op a b) = do
  rta <- rtFromExpr a
  rtb <- rtFromExpr b
  pure $ RTArith op rta rtb
rtFromExpr (Var n) = lookupVar n (lift . deepEval >=> rtFromVal) (const $ pure $ RTVar n)
rtFromExpr (App f x) = do
  tf <- lift $ deferExpr f
  lift (force tf) >>= \case
    (VClosure arg body env) -> do
      tx <- lift $ deferExpr x
      local (const $ bindThunk arg tx env) $
        lift (deepEvalExpr body) >>= rtFromVal
    (VFunc env argNames body) ->
      case x of
        List argExprs -> do
          rtArgs <- traverse rtFromExpr (toList argExprs)
          let name = "function_" <> show tf
          tell env
          tellFunction name (toList argNames) body
          pure $ RTCall name rtArgs
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    _ -> throwError "Calling a non-function"
rtFromExpr expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Lit {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal
rtFromExpr expr@Func {} = lift (deepEvalExpr expr) >>= rtFromVal

-- (VFunc arg body) ->
--   let name = "function_" <> show tf
--    in pure
-- TODO move to rtFromExpr where clause to emphasize that it is not used elsewhere
-- TODO handle(tell) the closure form VBlock
rtFromVal :: Value -> RTEval RTExpr
rtFromVal (Fix (VInt n)) = pure $ RTLit n
rtFromVal (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal (Fix VAttr {}) = throwError "can't handle attribute set values yet"
rtFromVal (Fix VList {}) = throwError "can't handle list values yet"
rtFromVal (Fix VFunc {}) = throwError "Function values don't make sense here"
rtFromVal (Fix (VBlock env b)) = RTBlock b <$ tell env
rtFromVal (Fix (VRTVar n)) = pure $ RTVar n

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
