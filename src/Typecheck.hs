{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TupleSections #-}

module Typecheck (typecheckBlock, typecheckFunction) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.ST
import Data.Map (Map)
import Data.Map qualified as M
import Data.UnionFind.ST qualified as UF
import Expr
import Lens.Micro.Platform
import Program

newtype TypeVar s info = TypeVar (UF.Point s (Map Type info))

data TypecheckContext s info = TypecheckContext
  { _tcVars :: Map Name (TypeVar s info),
    _tcBlock :: TypeVar s info,
    _tcNamedBlocks :: Map Name (TypeVar s info),
    _tcSelf :: SelfCtx,
    _tcDepth :: Int,
    _tcDeps :: Dependencies
  }

type Typecheck s info = ReaderT (TypecheckContext s info) (ST s)

type SelfCtx = [([Type], Type)]

makeLenses ''TypecheckContext

typecheckBlock ::
  Dependencies ->
  Int ->
  [([Type], Type)] ->
  RTBlock PreCall (Maybe Type) ->
  Either String (Type, RTBlock PreCall Type)
typecheckBlock deps depth selfs (Block mlbl stmts) = runTypecheckT deps depth selfs setup run
  where
    setup = (,mlbl,Nothing) <$> fresh'
    run = (fmap . fmap . fmap) (Block mlbl) (typecheck stmts)

typecheckFunction ::
  Dependencies ->
  Int ->
  [([Type], Type)] ->
  [(Name, Type)] ->
  Type ->
  RTBlock PreCall (Maybe Type) ->
  Either String (RTBlock PreCall Type)
typecheckFunction deps depth self args ret (Block mlbl stmts) = runTypecheckT deps depth self setup run
  where
    setup = do
      vRet <- fresh'
      setType' vRet ret ()
      args' <- (traverse . traverse) (flip freshFrom' ()) args
      pure (vRet, mlbl, Just (args', vRet))
    run =
      (fmap . fmap)
        (\(_, stmts') -> Block mlbl stmts')
        (typecheck stmts)

rtStmtTypes ::
  Traversal
    (Stmt typ (RTExpr PreCall typ typ))
    (Stmt typ' (RTExpr PreCall typ' typ'))
    typ
    typ'
rtStmtTypes f = stmtMasterTraversal (rtExprMasterTraversal pure f f pure pure) f pure

typecheck ::
  [Stmt (Maybe Type) (RTExpr PreCall (Maybe Type) (Maybe Type))] ->
  Typecheck s () (Either String (Type, [Stmt Type (RTExpr PreCall Type Type)]))
typecheck stmts = do
  stmts' <- checkStmts stmts
  runExceptT $ do
    tRet <- view tcBlock >>= getTypeInExceptT
    tStmts <- (traverse . rtStmtTypes) getTypeInExceptT stmts'
    pure (tRet, tStmts)

checkStmts ::
  [Stmt (Maybe Type) (RTExpr PreCall (Maybe Type) (Maybe Type))] ->
  Typecheck s () [Stmt (TypeVar s ()) (RTExpr PreCall (TypeVar s ()) (TypeVar s ()))]
checkStmts [] = pure []
checkStmts (Return expr : r) = do
  ret <-
    view tcSelf >>= \case
      ((_, ret) : _) -> pure ret
      [] -> error "Returning without a known return type"
  expr' <- checkRTExpr expr
  setType (rtInfo expr') ret ()
  r' <- checkStmts r
  pure (Return expr' : r')
checkStmts (Break mlbl mexpr : r) = do
  var <- case mlbl of
    Nothing -> view tcBlock
    Just lbl ->
      view (tcNamedBlocks . at lbl) >>= \case
        Nothing -> error "impossible?"
        Just x -> pure x
  mexpr' <- case mexpr of
    Nothing -> Nothing <$ setType var TVoid ()
    Just expr -> do
      expr' <- checkRTExpr expr
      unify var (rtInfo expr')
      pure $ Just expr'
  r' <- checkStmts r
  pure (Break mlbl mexpr' : r')
checkStmts (Continue mlbl : r) = do
  r' <- checkStmts r
  pure (Continue mlbl : r')
checkStmts (Decl name mtyp expr : r) = do
  var <- freshMaybe mtyp ()
  expr' <- checkRTExpr expr
  unify var (rtInfo expr')
  r' <- bind name var $ checkStmts r
  pure (Decl name var expr' : r')
checkStmts (Assign name expr : r) = do
  expr' <- checkRTExpr expr
  view (tcVars . at name) >>= \case
    Nothing -> error "you're referencing a function that doesn't exit in the environment, this should be impossible"
    Just var -> unify var (rtInfo expr')
  r' <- checkStmts r
  pure (Assign name expr' : r')
checkStmts (ExprStmt expr : r) = do
  expr' <- checkRTExpr expr
  r' <- checkStmts r
  pure (ExprStmt expr' : r')

checkRTExpr ::
  RTExpr PreCall (Maybe Type) (Maybe Type) ->
  Typecheck s () (RTExpr PreCall (TypeVar s ()) (TypeVar s ()))
checkRTExpr (RTVar name mtyp) = do
  var <- freshMaybe mtyp ()
  view (tcVars . at name) >>= \case
    Nothing -> pure (RTVar name var)
    Just var' -> RTVar name var <$ unify var var'
checkRTExpr (RTLiteral lit mtyp) = RTLiteral lit <$> freshMaybe mtyp ()
checkRTExpr (RTArith op a b mtyp) = do
  var <- freshMaybe mtyp ()
  a' <- checkRTExpr a
  b' <- checkRTExpr b
  unify var (rtInfo a')
  unify (rtInfo a') (rtInfo b')
  pure $ RTArith op a' b' var
checkRTExpr (RTBlock (Block mlbl stmts) mtyp) = do
  var <- freshMaybe mtyp ()
  stmts' <-
    local (tcBlock .~ var) $
      maybeBindBlock var mlbl $
        checkStmts stmts
  pure (RTBlock (Block mlbl stmts') var)
checkRTExpr (RTCall call args mtyp) = do
  (cargs, cret) <- callSig call
  var <- freshMaybe mtyp ()
  setType var cret ()
  let f expr typ = do
        expr' <- checkRTExpr expr
        setType (rtInfo expr') typ ()
        pure expr'
  when (length args /= length cargs) $ error "argument length mismatch" -- TODO throw an error
  args' <- zipWithM f args cargs
  pure $ RTCall call args' var

callSig :: PreCall -> Typecheck s () ([Type], Type)
callSig (CallKnown guid) = do
  view (tcDeps . depKnownFuncs . at guid) >>= \case
    Nothing -> error "impossible"
    Just fn -> pure (fn ^.. fnArgs . traverse . _2, fn ^. fnRet)
callSig (CallRec n) = do
  depth <- view tcDepth
  sel <- view tcSelf
  view (tcSelf . to (safeLookup (depth - n))) >>= \case
    Nothing -> error $ "impossible: " <> show (n, depth, depth - n, sel)
    Just t -> pure t
callSig (CallTemp tid) = do
  view (tcDeps . depTempFuncs . at tid) >>= \case
    Nothing -> error "impossible"
    Just (TempFunc fn _) -> pure (fn ^.. fnArgs . traverse . _2, fn ^. fnRet)

safeLookup :: Int -> [a] -> Maybe a
safeLookup n l = case splitAt n l of
  (_, a : _) -> Just a
  _ -> Nothing

freshFrom' :: Type -> info -> ST s (TypeVar s info)
freshFrom' typ info = do
  var <- fresh'
  setType' var typ info
  pure var

-- freshFrom :: Type -> info -> Typecheck s info (TypeVar s info)
-- freshFrom typ info = lift $ freshFrom' typ info

freshMaybe :: Maybe Type -> info -> Typecheck s info (TypeVar s info)
freshMaybe mtyp info = do
  var <- fresh
  forM_ mtyp $ \typ -> setType var typ info
  pure var

getTypeInExceptT :: TypeVar s info -> ExceptT String (Typecheck s info) Type
getTypeInExceptT var = do
  lift (checkType var) >>= \case
    [(a, _)] -> pure a
    [] -> pure TVoid
    l -> throwError $ "Overdetermined: " <> show (fst <$> l)

bind :: Name -> TypeVar s info -> Typecheck s info a -> Typecheck s info a
bind n t = local (over tcVars $ M.insert n t)

fresh :: Typecheck s info (TypeVar s info)
fresh = lift fresh'

unify :: Monoid info => TypeVar s info -> TypeVar s info -> Typecheck s info ()
unify ta tb = lift $ unify' ta tb

setType :: TypeVar s info -> Type -> info -> Typecheck s info ()
setType var typ info = lift $ setType' var typ info

fresh' :: ST s (TypeVar s info)
fresh' = TypeVar <$> UF.fresh mempty

unify' :: Monoid info => TypeVar s info -> TypeVar s info -> ST s ()
unify' (TypeVar ta) (TypeVar tb) = UF.union' ta tb (\sa sb -> pure (M.unionWith mappend sa sb))

setType' :: TypeVar s info -> Type -> info -> ST s ()
setType' (TypeVar t) typ info = UF.modifyDescriptor t (M.insert typ info)

maybeBindBlock ::
  TypeVar s info ->
  Maybe Name ->
  Typecheck s info a ->
  Typecheck s info a
maybeBindBlock _ Nothing = id
maybeBindBlock var (Just lbl) = local (tcNamedBlocks . at lbl ?~ var)

runTypecheckT ::
  Dependencies ->
  Int ->
  [([Type], Type)] ->
  (forall s. ST s (TypeVar s info, Maybe Name, Maybe ([(Name, TypeVar s info)], TypeVar s info))) ->
  (forall s. Typecheck s info a) ->
  a
runTypecheckT deps depth selfs setup run = runST $ do
  (blk, mlbl, mfun) <- setup
  let ctx = TypecheckContext mempty blk mempty selfs depth deps
  let args = maybe [] fst mfun
  flip runReaderT ctx $
    maybeBindBlock blk mlbl $
      foldr (uncurry bind) run args

checkType :: TypeVar s info -> Typecheck s info [(Type, info)]
checkType var = M.toList <$> tlookup var
  where
    tlookup :: TypeVar s info -> Typecheck s info (Map Type info)
    tlookup (TypeVar p) = lift $ UF.repr p >>= UF.descriptor
