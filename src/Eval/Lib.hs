{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO export list
-- TODO Things not related to any of the eval types should probably be moved to a more general Eval module
module Eval.Lib where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer.Lazy
import Data.Foldable (toList)
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Text (Text)
import Data.Text qualified as T
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

close :: Applicative n => ReaderT r m a -> ReaderT r n (m a)
close (ReaderT f) = ReaderT $ \r -> pure (f r)

labels :: RTAST f => Traversal (f var lbl fun lit info) (f var lbl' fun lit info) lbl lbl'
labels f = traverseAst pure f pure (const pure) pure

vars :: RTAST f => Traversal (f var lbl fun lit info) (f var' lbl fun lit info) var var'
vars f = traverseAst f pure pure (const pure) pure

types :: RTAST f => Traversal (f var lbl fun lit info) (f var lbl fun lit info') info info'
types = traverseAst pure pure pure (const pure)

calls :: RTAST f => Traversal (f var lbl fun lit info) (f var lbl fun' lit info) fun fun'
calls f = traverseAst pure pure f (const pure) pure

lits :: RTAST f => Traversal (f var lbl fun lit inf) (f var lbl fun lit' inf) lit lit'
lits f = traverseAst pure pure pure (const f) pure

rtFuncCalls :: Traversal (RTFunc a) (RTFunc b) a b
rtFuncCalls f (RTFunc a r b) = RTFunc a r <$> traverseAst pure pure f (const pure) pure b

-- TODO Describe primitives in more detail
describeValue :: Value f -> String
describeValue VClosure {} = "a closure"
describeValue VRTValue {} = "a runtime expression"
describeValue VRTPlace {} = "a runtime variable"
describeValue VFunc {} = "a runtime function"
describeValue VType {} = "a type"
describeValue VPrim {} = "a primitive"
describeValue VBlk {} = "a runtime block"
describeValue VAttr {} = "an attribute set"
describeValue VString {} = "a string"
describeValue VList {} = "a list"

lookupName :: Symbol -> Eval Thunk
lookupName name =
  view (binds . at name) >>= \case
    Nothing -> throwError $ "Unknown variable: " <> show name
    Just t -> pure t

defer :: MonadIO m => EvalBase WHNF -> m Thunk
defer = fmap Thunk . liftIO . newIORef . Left

refer :: MonadIO m => WHNF -> m Thunk
refer = fmap Thunk . liftIO . newIORef . Right

overrideThunk :: MonadIO m => Thunk -> Either (EvalBase WHNF) WHNF -> m ()
overrideThunk (Thunk ref) val = liftIO $ writeIORef ref val

force :: Thunk -> EvalBase WHNF
force (Thunk ref) = do
  liftIO (readIORef ref) >>= \case
    Right a -> pure a
    Left m -> do
      liftIO $ writeIORef ref (Left $ throwError "infinite recursion")
      a <- m
      liftIO $ writeIORef ref (Right a)
      pure a

fresh :: MonadFresh m => m Int
fresh = state $ \n -> (n, n + 1)

freshBlock :: MonadFresh m => m BlockIX
freshBlock = BlockIX <$> fresh

freshVar :: MonadFresh m => m VarIX
freshVar = VarIX <$> fresh

freshFunc :: MonadFresh m => m FuncIX
freshFunc = FuncIX <$> fresh

-- TODO prisms?
ensureValue :: MonadError String m => String -> (WHNF -> Maybe r) -> WHNF -> m r
ensureValue ex f v = case f v of
  Just r -> pure r
  Nothing -> throwError $ "Expected " <> ex <> ", but got " <> describeValue v

ensureType :: MonadError String m => WHNF -> m Type
ensureType = ensureValue "a type" $ \case
  VType typ -> Just typ
  _ -> Nothing

ensureBlock :: MonadError String m => WHNF -> m BlockIX
ensureBlock = ensureValue "a block" $ \case
  VBlk blk -> Just blk
  _ -> Nothing

fromComp :: (Deps -> a -> r) -> Comp a -> Eval r
fromComp f m = (\(a, dep) -> f dep a) <$> runWriterT m

-- TODO This is one of the situations in which we see that Text is not great
-- for our String; indexing and lookups are O(n)
indexMaybe :: Text -> Int -> Maybe Char
indexMaybe ps n
  | n < 0 = Nothing
  | n >= T.length ps = Nothing
  | otherwise = Just $! T.index ps n
{-# INLINE indexMaybe #-}

withError :: MonadError e m => e -> m a -> m a
withError err m = catchError m (const $ throwError err)

coerceRTValue :: WHNF -> Comp (EvalPhase RTValue)
coerceRTValue (VRTValue deps val) = val <$ tell deps
coerceRTValue (VRTPlace deps plc) = PlaceVal plc () <$ tell deps
coerceRTValue (VPrim prim) = pure (RTLit prim ())
coerceRTValue (VList l) = flip RTTuple () <$> traverse (lift . lift . force >=> coerceRTValue) l
coerceRTValue val = throwError ("Could not coerce " <> describeValue val <> " into a runtime value")

coerceRTPlace :: WHNF -> Comp (EvalPhase RTPlace)
coerceRTPlace (VRTPlace deps plc) = plc <$ tell deps
coerceRTPlace val = throwError $ "Could not coerce " <> describeValue val <> " into a runtime place expression"

mkCallGraph :: FuncIX -> RTFunc (Either FuncIX Hash) -> Set CallGraph -> CallGraph
mkCallGraph ix body deps = CallGraph ix body (S.difference open bind) bind deps
  where
    bind = S.insert ix $ foldMap cgBind deps
    open = foldMap cgOpen deps <> S.fromList (body ^.. rtFuncCalls . _Left)

closeFunc :: CallGraph -> Maybe (Hash, HashMap Hash (RTFunc Hash))
closeFunc cg
  | S.null (cgOpen cg) = Just $ either (error "impossible") id $ flattenCallgraph cg
  | otherwise = Nothing

hashCallGraph :: CallGraph -> Hash
hashCallGraph cg = Hash $ evalState (go cg) (mempty, 0)
  where
    label :: FuncIX -> State (Map FuncIX Int, Int) Int
    label ix =
      use (_1 . at ix) >>= \case
        Nothing -> state $ \(m, n) -> (n, (M.insert ix n m, n + 1))
        Just n -> pure n
    go :: CallGraph -> State (Map FuncIX Int, Int) Int
    go (CallGraph ix body _ _ deps) = do
      _ <- label ix
      body' <- (rtFuncCalls . _Left) label body
      hDeps <- traverse go (S.toAscList deps)
      pure $ hash (body', hDeps)

flattenCallgraph :: CallGraph -> Either FuncIX (Hash, HashMap Hash (RTFunc Hash))
flattenCallgraph cg = evalRWST (go cg) () mempty
  where
    go ::
      CallGraph ->
      ( RWST
          ()
          (HashMap Hash (RTFunc Hash))
          (Map FuncIX Hash)
          (Either FuncIX)
      )
        Hash
    go cg@(CallGraph ix body _ _ deps) = do
      let h = hashCallGraph cg
      at ix ?= h
      forM_ (S.toList deps) go
      body' <-
        let f ix = use (at ix) >>= maybe (throwError ix) pure
         in rtFuncCalls (either f pure) body
      tell $ HM.singleton h body'
      pure h

liftLocal :: MonadReader r m => (r -> r) -> ReaderT r' m a -> ReaderT r' m a
liftLocal f = mapReaderT (local f)

extractVal :: RTValue var blk fun lit info -> info
extractVal (RTArith _ _ _ i) = i
extractVal (RTComp _ _ _ i) = i
extractVal (RTLit _ i) = i
extractVal (ValueSel _ _ i) = i
extractVal (RTTuple _ i) = i
extractVal (RTCond _ _ _ i) = i
extractVal (Call _ _ i) = i
extractVal (PlaceVal _ i) = i
extractVal (Block _ i) = i
extractVal (RTRef _ i) = i

extractPlace :: RTPlace var blk fun lit info -> info
extractPlace (Place _ i) = i
extractPlace (PlaceSel _ _ i) = i
extractPlace (RTDeref _ i) = i

-- TODO maybe move to a fixpoint module?
foldNF :: (Value r -> r) -> NF -> r
foldNF f = go where go (NF v) = f (go <$> v)

foldMNF :: Monad m => (Value r -> m r) -> NF -> m r
foldMNF f = go
  where
    go (NF v) = traverse go v >>= f

foldType :: (TypeF r -> r) -> Type -> r
foldType f = go where go (Type t) = f (go <$> t)

structuralZip :: (Eq (t ()), Applicative m, Traversable t) => (a -> b -> m c) -> m (t c) -> t a -> t b -> m (t c)
structuralZip f s0 a b =
  if (() <$ a) == (() <$ b)
    then substitute a <$> zipWithM f (toList a) (toList b)
    else s0
  where
    substitute t = evalState (traverse (const $ state $ \case (e : es) -> (e, es); [] -> error "impossible") t)

unEval :: Thunk -> Eval a -> EvalBase a
unEval tBuiltins = flip runReaderT env0
  where
    env0 = EvalEnv (M.singleton "builtins" tBuiltins)
