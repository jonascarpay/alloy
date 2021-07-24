{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- TODO export list
module Eval.Lib where

import Control.Monad.Except
import Control.Monad.RWS
import Control.Monad.Reader
import Control.Monad.State
import Control.Monad.Writer (runWriterT)
import Control.Monad.Writer.Lazy
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BSU
import Data.HashMap.Strict (HashMap)
import Data.HashMap.Strict qualified as HM
import Data.Hashable
import Data.IORef
import Data.List (sort)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Set (Set)
import Data.Set qualified as S
import Data.Void
import Data.Word (Word8)
import Eval.Lenses
import Eval.Types
import Expr
import Lens.Micro.Platform hiding (ix)

close :: Applicative n => ReaderT r m a -> ReaderT r n (m a)
close (ReaderT f) = ReaderT $ \r -> pure (f r)

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

{-# INLINE asRTValue #-}
asRTValue :: WHNF -> Maybe (Comp (RTValue VarIX BlockIX (Either FuncIX Hash)))
asRTValue (VRTValue deps val) = Just $ val <$ tell deps
asRTValue (VRTPlace deps plc) = Just $ PlaceVal plc <$ tell deps
asRTValue (VPrim prim) = Just $ pure $ RTPrim prim
asRTValue _ = Nothing

coerceRTValue :: WHNF -> Comp (RTValue VarIX BlockIX (Either FuncIX Hash))
coerceRTValue val = fromMaybe (throwError $ "Could not coerce " <> describeValue val <> " into a runtime value") $ asRTValue val

{-# INLINE asRTPlace #-}
-- TODO do we allow changing PlaceVal back into Place?
asRTPlace :: WHNF -> Maybe (Comp (RTPlace VarIX BlockIX (Either FuncIX Hash)))
asRTPlace (VRTPlace deps plc) = Just $ plc <$ tell deps
asRTPlace _ = Nothing

coerceRTPlace :: WHNF -> Comp (RTPlace VarIX BlockIX (Either FuncIX Hash))
coerceRTPlace plc = fromMaybe (throwError $ "Could not coerce " <> describeValue plc <> " into a runtime place expression") $ asRTPlace plc

mkCallGraph :: FuncIX -> RTFunc (Either FuncIX Hash) -> Set CallGraph -> CallGraph
mkCallGraph ix body deps = CallGraph ix body (S.difference open bind) bind deps
  where
    bind = S.insert ix $ foldMap cgBind deps
    open = foldMap cgOpen deps <> S.fromList (body ^.. traverse . _Left)

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
      body' <- (traverse . _Left) label body
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
         in traverse (either f pure) body
      tell $ HM.singleton h body'
      pure h
