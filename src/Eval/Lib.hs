{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

-- TODO export list
module Eval.Lib where

import Control.Monad.Except
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
import Data.Map qualified as M
import Data.Maybe (fromMaybe)
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

closeFunc :: TempFunc (Either FuncIX Hash) -> Maybe (HashMap Hash (RTFunc Hash), Hash)
closeFunc temp = flattenTemp . fmap (either absurd id) <$> closedOver (traverse . _Left) temp

hashTempFunc :: (Hashable a, Ord a) => TempFunc a -> Hash
hashTempFunc (TempFunc fn deps) = Hash $ hash (fn, fmap hashTempFunc . sort $ M.elems deps)

flattenTemp :: TempFunc Hash -> (HashMap Hash (RTFunc Hash), Hash)
flattenTemp tree@(TempFunc fn deps) = (HM.singleton guid fn' <> transitive, guid)
  where
    guid = hashTempFunc tree
    transitive = foldMap (fst . flattenTemp . instantiate1Over traverse guid) (M.elems deps)
    fn' = instantiate1Over traverse guid fn
