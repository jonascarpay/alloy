{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
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
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.Map (Map)
import Data.Map qualified as M
import Data.Set (Set)
import Data.Set qualified as S
import Data.Word (Word8)
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

close :: Applicative n => ReaderT r m a -> ReaderT r n (m a)
close (ReaderT f) = ReaderT $ \r -> pure (f r)

labels :: RTAST f => Traversal (f var lbl fun info) (f var lbl' fun info) lbl lbl'
labels f = traverseAst pure f pure pure

vars :: RTAST f => Traversal (f var lbl fun info) (f var' lbl fun info) var var'
vars f = traverseAst f pure pure pure

types :: RTAST f => Traversal (f var lbl fun info) (f var lbl fun info') info info'
types = traverseAst pure pure pure

calls :: RTAST f => Traversal (f var lbl fun info) (f var lbl fun' info) fun fun'
calls f = traverseAst pure pure f pure

rtFuncCalls :: Traversal (RTFunc a) (RTFunc b) a b
rtFuncCalls f (RTFunc a r b) = RTFunc a r <$> traverseAst pure pure f pure b

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

lookupName :: Name -> Eval Thunk
lookupName name =
  view (binds . at name) >>= \case
    Nothing -> throwError $ "Unknown variable: " <> show name
    Just t -> pure t

defer :: MonadIO m => EvalBase WHNF -> m Thunk
defer = fmap Thunk . liftIO . newIORef . Left

refer :: MonadIO m => WHNF -> m Thunk
refer = fmap Thunk . liftIO . newIORef . Right

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

abstractOver :: Traversal s t a (Bind b a) -> (a -> Maybe b) -> s -> t
abstractOver t f = over t (\a -> maybe (Free a) Bound (f a))

abstract1Over :: Eq a => Traversal s t a (Bind () a) -> a -> s -> t
abstract1Over t a = over t (\var -> if var == a then Bound () else Free var)

closedOver :: Traversal s t a b -> s -> Maybe t
closedOver t = t (const Nothing)

instantiateOver :: Traversal s t (Bind b a) a -> (b -> a) -> s -> t
instantiateOver t f = over t $ \case
  Free a -> a
  Bound b -> f b

instantiate1Over :: Traversal s t (Bind () a) a -> a -> s -> t
instantiate1Over t sub = over t $ \case
  Free a -> a
  Bound () -> sub

unbind :: (b -> r) -> (a -> r) -> (Bind b a -> r)
unbind fb _ (Bound b) = fb b
unbind _ fa (Free a) = fa a

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

-- TODO replace with indexMaybe from BS 0.11
indexMaybe :: ByteString -> Int -> Maybe Word8
indexMaybe ps n
  | n < 0 = Nothing
  | n >= BS.length ps = Nothing
  | otherwise = Just $! BSU.unsafeIndex ps n
{-# INLINE indexMaybe #-}

withError :: MonadError e m => e -> m a -> m a
withError err m = catchError m (const $ throwError err)

coerceRTValue :: WHNF -> Comp (EvalPhase RTValue)
coerceRTValue (VRTValue deps val) = val <$ tell deps
coerceRTValue (VRTPlace deps plc) = PlaceVal plc () <$ tell deps
coerceRTValue (VPrim prim) = pure (RTPrim prim ())
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

-- Comonad-y stuff

extractVal :: RTValue var blk fun info -> info
extractVal (RTArith _ _ _ i) = i
extractVal (RTComp _ _ _ i) = i
extractVal (RTPrim _ i) = i
extractVal (ValueSel _ _ i) = i
extractVal (RTTuple _ i) = i
extractVal (RTCond _ _ _ i) = i
extractVal (Call _ _ i) = i
extractVal (PlaceVal _ i) = i
extractVal (Block _ i) = i
extractVal (RTRef _ i) = i

extractPlace :: RTPlace var blk fun info -> info
extractPlace (Place _ i) = i
extractPlace (PlaceSel _ _ i) = i
extractPlace (RTDeref _ i) = i

extendVal ::
  (forall var blk fun. RTValue var blk fun a -> b) ->
  (forall var blk fun. RTPlace var blk fun a -> b) ->
  RTValue var blk fun a ->
  RTValue var blk fun b
extendVal fv fp = go
  where
    go s@(RTArith op a b _) = RTArith op (go a) (go b) (fv s)
    go s@(RTComp op a b _) = RTComp op (go a) (go b) (fv s)
    go s@(RTPrim p _) = RTPrim p (fv s)
    go s@(ValueSel h n _) = ValueSel (go h) n (fv s)
    go s@(RTTuple t _) = RTTuple (go <$> t) (fv s)
    go s@(RTCond c t f _) = RTCond (go c) (go t) (go f) (fv s)
    go s@(Call f args _) = Call f (go <$> args) (fv s)
    go s@(PlaceVal pl _) = PlaceVal (extendPlace fv fp pl) (fv s)
    go s@(Block blk _) = Block (extendProg fv fp blk) (fv s)
    go s@(RTRef p _) = RTRef (extendPlace fv fp p) (fv s)

extendPlace ::
  (forall var blk fun. RTValue var blk fun a -> b) ->
  (forall var blk fun. RTPlace var blk fun a -> b) ->
  RTPlace var blk fun a ->
  RTPlace var blk fun b
extendPlace fv fp = go
  where
    go s@(Place var _) = Place var (fp s)
    go s@(PlaceSel h n _) = PlaceSel (go h) n (fp s)
    go s@(RTDeref v _) = RTDeref (extendVal fv fp v) (fp s)

extendProg ::
  (forall var blk fun. RTValue var blk fun a -> b) ->
  (forall var blk fun. RTPlace var blk fun a -> b) ->
  RTProg var blk fun a ->
  RTProg var blk fun b
extendProg fv fp (Decl m v k) = Decl m (extendVal fv fp v) (extendProg fv fp k)
extendProg fv fp (Assign lhs rhs k) = Assign (extendPlace fv fp lhs) (extendVal fv fp rhs) (extendProg fv fp k)
extendProg fv fp (Break lbl val) = Break lbl (extendVal fv fp val)
extendProg _ _ (Continue lbl) = Continue lbl
extendProg fv fp (ExprStmt expr mk) = ExprStmt (extendVal fv fp expr) (extendProg fv fp <$> mk)

-- TODO maybe move to a fixpoint module?
foldNF :: (Value r -> r) -> NF -> r
foldNF f = go where go (NF v) = f (go <$> v)

foldMNF :: Monad m => (Value r -> m r) -> NF -> m r
foldMNF f = go
  where
    go (NF v) = traverse go v >>= f
