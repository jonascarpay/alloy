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
import Data.Void
import Expr
import Lens.Micro.Platform
import Program

type ThunkID = Int

--TODO Closure Args (p -> m r)
--TODO How to properly handle RT Vars
--TODO I think spliced variables can escape their scope?
-- maybe just tag them from fresh to make sure
data ValueF val
  = VPrim Prim
  | VClosure Name Expr Env
  | VType Type
  | VAttr (Map Name val)
  | VRTVar (Type, Name)
  | VBlock RuntimeEnv Type (Block Type (RTExpr Type)) -- TODO Move Type into Block?
  | VFunc RuntimeEnv (Function Type (RTExpr Type)) -- TODO Type here could be a val?
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

type Env = Map Name (Either ThunkID Type)

lookupVar :: (MonadReader Env m, MonadError String m) => Name -> (ThunkID -> m r) -> (Type -> m r) -> m r
lookupVar name kct krt = do
  asks (M.lookup name) >>= \case
    Nothing -> throwError $ "undefined variable " <> show name
    Just (Left tid) -> kct tid
    Just (Right typ) -> krt typ

bindThunk :: Name -> ThunkID -> Env -> Env
bindThunk n tid = M.insert n (Left tid)

bindRtvar :: Name -> Type -> Env -> Env
bindRtvar n typ = M.insert n (Right typ)

yCombinator :: Expr
yCombinator = Lam "f" (App (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))) (Lam "x" (App (Var "f") (App (Var "x") (Var "x")))))

(<?>) :: MonadError e m => Maybe a -> e -> m a
(<?>) m e = maybe (throwError e) pure m

eval :: Expr -> Either String Value
eval eRoot = runEval $ withBuiltins $ deepEvalExpr eRoot

runEval :: Eval a -> Either String a
runEval (EvalT m) = fmap fst $ runExcept $ evalRWST m mempty (0, mempty)

deferAttrs :: [(Name, ValueF Void)] -> Eval ThunkID
deferAttrs attrs = do
  attrs' <- (traverse . traverse) (deferVal . fmap absurd) attrs
  deferVal $ VAttr $ M.fromList attrs'

withBuiltins :: Eval a -> Eval a
withBuiltins m = do
  tUndefined <- deferM $ throwError "undefined"
  tNine <- deferVal . VPrim $ PInt 9
  tFix <- deferExpr yCombinator
  tDouble <- deferVal (VType TDouble)
  tInt <- deferVal (VType TInt)
  tVoid <- deferVal (VType TVoid)
  tBuiltins <-
    deferVal . VAttr $
      M.fromList
        [ ("undefined", tUndefined),
          ("nine", tNine),
          ("fix", tFix),
          ("double", tDouble),
          ("int", tInt),
          ("void", tVoid)
        ]
  local (bindThunk "builtins" tBuiltins) m

fromPrimitive :: Type -> ValueF a -> Eval (RTExpr Type)
fromPrimitive TInt (VPrim (PInt n)) = pure $ RTPrim (PInt n) TInt
fromPrimitive TDouble (VPrim (PInt n)) = pure $ RTPrim (PDouble $ fromIntegral n) TDouble
fromPrimitive TDouble (VPrim (PDouble n)) = pure $ RTPrim (PDouble n) TDouble
fromPrimitive trg (VPrim src) = throwError $ "Cannot convert a " <> show src <> " primitive into type " <> show trg
fromPrimitive trg _ = throwError $ "Cannot construct a " <> show trg <> " from whatever it is your passing "

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
    _ -> throwError "Calling a non-function"
step (Var x) = lookupVar x force (\t -> pure $ VRTVar (t, x))
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
step (BlockExpr b) = (\(env, typ, b') -> VBlock env typ b') <$> genBlock Nothing b
step (List l) = VList <$> traverse deferExpr l
step (Func args ret body) = do
  typedArgs <- (traverse . traverse) evalType args
  retType <- evalType ret
  local (flip (foldr (uncurry bindRtvar)) typedArgs . forgetRT) $ do
    (env, btyp, blk) <- genBlock (Just retType) body
    pure $ VFunc env (Function typedArgs btyp blk)

forgetRT :: Env -> Env
forgetRT = M.filter isLeft

evalType :: Expr -> Eval Type
evalType expr =
  step expr >>= \case
    VType tp -> pure tp
    _ -> throwError "Expected type, got not-a-type"

genBlock :: Maybe Type -> Block Expr Expr -> Eval (RuntimeEnv, Type, Block Type (RTExpr Type))
-- genBlock (Block stmts) = (\(rtStmts, env) -> (env, Block rtStmts)) <$> runWriterT (genStmt stmts)
genBlock mtyp (Block stmts) = do
  ((typ, stmts'), env) <- runWriterT $ genStmt mtyp stmts
  pure (env, typ, Block stmts')

-- pure (env, typ, Block blk)

type RTEval = WriterT RuntimeEnv Eval -- TODO rename this

tellFunction :: Name -> [(Name, Type)] -> Type -> Block Type (RTExpr Type) -> RTEval ()
tellFunction name args ret body = tell (RuntimeEnv $ M.singleton name (Function args ret body))

-- TODO this whole thing is probably better off being a State
genStmt :: Maybe Type -> [Stmt Expr Expr] -> RTEval (Type, [Stmt Type (RTExpr Type)])
genStmt btyp [Return expr] = do
  rtExpr <- rtFromExpr btyp expr
  pure (rtType rtExpr, [Return rtExpr])
genStmt _ (Return _ : _) = throwError "Return is not final expression in block"
genStmt btyp (Decl name typ expr : r) = do
  typ' <- lift $ evalType typ
  expr' <- rtFromExpr (Just typ') expr
  (ret, r') <- local (bindRtvar name typ') (genStmt btyp r)
  pure (ret, Decl name typ' expr' : r')
genStmt btyp (Assign name expr : r) = do
  (vtyp, name') <- do
    let ct tid =
          force tid >>= \case
            VRTVar v -> pure v
            _ -> throwError $ "Assigning to non-runtime-variable " <> show name
        rt vtyp = pure (vtyp, name)
     in lift $ lookupVar name ct rt
  expr' <- rtFromExpr (Just vtyp) expr
  (ret, r') <- genStmt btyp r
  pure (ret, Assign name' expr' : r')
genStmt btyp (ExprStmt expr : r) = do
  expr' <- rtFromExpr Nothing expr
  (ret, r') <- genStmt btyp r
  pure (ret, ExprStmt expr' : r')
genStmt _ [] = pure (TVoid, [])

unify :: MonadError String m => Maybe Type -> Maybe Type -> m Type
unify Nothing Nothing = throwError "ambiguous"
unify (Just t) Nothing = pure t
unify Nothing (Just t) = pure t
unify (Just ta) (Just tb) =
  if ta == tb
    then pure ta
    else throwError $ "Couldnt match " <> show ta <> " with " <> show tb

unifyExpr :: RTExpr Type -> RTExpr Type -> RTEval Type
unifyExpr rta rtb =
  let ta = rtType rta
      tb = rtType rtb
   in unify (Just ta) (Just tb)

-- TODO document why this is split into rtFromExpr and rtFromVal
-- TODO see if there is potential unification
-- TODO really this is just for reusing arithmetic ops (and vars?) I think which may be unnecessary
rtFromExpr :: Maybe Type -> Expr -> RTEval (RTExpr Type)
rtFromExpr typ (Arith op a b) = do
  rta <- rtFromExpr typ a
  rtb <- rtFromExpr typ b
  typ' <- unifyExpr rta rtb
  pure $ RTArith op rta rtb typ'
rtFromExpr typ (Var n) = lookupVar n (lift . deepEval >=> rtFromVal typ) (fmap (RTVar n) . unify typ . Just)
rtFromExpr typ (App f x) = do
  tf <- lift $ deferExpr f
  lift (force tf) >>= \case
    (VClosure arg body env) -> do
      tx <- lift $ deferExpr x
      local (const $ bindThunk arg tx env) $
        lift (deepEvalExpr body) >>= rtFromVal typ
    (VFunc env (Function argDecls ret body)) ->
      case x of
        List argExprs -> do
          typ' <- unify typ (Just ret)
          rtArgs <- zipWithM rtFromExpr (fmap (Just . snd) argDecls) (toList argExprs)
          let name = "function_" <> show tf
          tell env
          tellFunction name (toList argDecls) ret body -- TODO warning this is tricky
          pure $ RTCall name rtArgs typ'
        _ -> throwError "Trying to call a function with a non-list-like-thing"
    _ -> throwError "Calling a non-function"
rtFromExpr typ expr@Lam {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@Prim {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@Let {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@Attr {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@List {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@Acc {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@BlockExpr {} = lift (deepEvalExpr expr) >>= rtFromVal typ
rtFromExpr typ expr@Func {} = lift (deepEvalExpr expr) >>= rtFromVal typ

-- (VFunc arg body) ->
--   let name = "function_" <> show tf
--    in pure
-- TODO move to rtFromExpr where clause to emphasize that it is not used elsewhere
-- TODO handle(tell) the closure form VBlock
rtFromVal :: Maybe Type -> Value -> RTEval (RTExpr Type)
rtFromVal (Just typ) (Fix v@(VPrim _)) = lift $ fromPrimitive typ v
rtFromVal Nothing (Fix (VPrim t)) = throwError $ "not sure what type to turn " <> show t <> " into"
rtFromVal _ (Fix VClosure {}) = throwError "partially applied closure in runtime expression"
rtFromVal _ (Fix VAttr {}) = throwError "can't handle attribute set values yet"
rtFromVal _ (Fix VList {}) = throwError "can't handle list values yet"
rtFromVal _ (Fix VType {}) = throwError "can't handle type"
rtFromVal _ (Fix VFunc {}) = throwError "Function values don't make sense here"
rtFromVal typ (Fix (VBlock env typblock b)) = do
  t' <- unify typ (Just typblock)
  RTBlock b t' <$ tell env
rtFromVal typ (Fix (VRTVar (t, n))) = RTVar n <$> unify typ (Just t)

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
