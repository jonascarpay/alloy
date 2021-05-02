{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}

module Builtins where

-- TODO Builtins are in this module only because matchType -> reduce -> step -> Expr

import Control.Monad.Except
import Control.Monad.RWS
import Data.Foldable
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Eval
import Expr
import Lens.Micro.Platform
import Parse
import System.FilePath
import Text.Megaparsec qualified as MP

bTypeOf :: ValueF ThunkID -> Eval (ValueF ThunkID)
bTypeOf (VRTVar tid) =
  view (envRTVar tid) >>= \case
    Just tv -> VType <$> getTypeSuspend tv
    Nothing -> throwError "Cannot get bTypeOf"
bTypeOf (VBlockLabel tid) =
  view (envRTLabel tid) >>= \case
    Just tv -> VType <$> getTypeSuspend tv
    Nothing -> throwError "Cannot get bTypeOf"
-- bTypeOf (VBlock _ blk) = VType <$> getTypeSuspend (blk ^. blkType)
bTypeOf _ = throwError "Cannot get bTypeOf"

bError :: ValueF ThunkID -> Eval (ValueF ThunkID)
bError (VPrim (PString msg)) = throwError $ "error: " <> msg
bError _ = throwError "builtins.error was passed a non-string"

attrNames :: ValueF ThunkID -> Eval (ValueF ThunkID)
attrNames (VAttr attrs) = do
  names <- traverse (deferVal . VPrim . PString) (M.keys attrs)
  pure $ VList $ Seq.fromList names
attrNames _ = throwError "attrNames on non-list"

-- TODO combine with normal field accessor
bLookup :: ValueF ThunkID -> ValueF ThunkID -> Eval (ValueF ThunkID)
bLookup (VAttr attrs) (VPrim (PString str)) =
  case M.lookup str attrs of
    Nothing -> throwError "field doesn't exist"
    Just v -> force v -- TODO this should be lazier
bLookup (VAttr _) _ = throwError "second argument to stringField was not a string"
bLookup _ _ = throwError "first argument to stringField was not an attr set"

bIndex :: ValueF ThunkID -> ValueF ThunkID -> Eval (ValueF ThunkID)
bIndex (VList xs) (VPrim (PInt i)) =
  case Seq.lookup i xs of
    Nothing -> throwError $ "builtins.index: index " <> show i <> " out of bounds"
    Just x -> force x
bIndex (VList _) _ = throwError "builtins.index called with not an integer"
bIndex _ _ = throwError "builtins.index called with not a list"

bLength :: ValueF ThunkID -> Eval (ValueF ThunkID)
bLength (VList xs) = pure $ VPrim $ PInt $ Seq.length xs
bLength (VPrim (PString s)) = pure $ VPrim $ PInt $ length s
bLength _ = throwError "builtins.length called with not a list or string"

bStruct :: ValueF ThunkID -> Eval (ValueF ThunkID)
bStruct (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
bStruct _ = throwError "Making a struct typedef from something that's not an attrset"

-- TODO this could be lazier if VClosure' were lazier
bImport :: (Expr -> Eval a) -> ValueF ThunkID -> Eval a
bImport f (VPrim (PString rel)) = do
  file <- view envFile
  let file' = takeDirectory file </> rel
  input <- liftIO $ readFile file'
  case MP.parse pToplevel file' input of
    Left err -> throwError $ MP.errorBundlePretty err
    Right expr -> local (envFile .~ file') $ f expr
bImport _ _ = throwError "import needs a filepath"

bListToAttrs :: ValueF ThunkID -> Eval (ValueF ThunkID)
bListToAttrs (VList xs) = do
  pairs <- forM (toList xs) $ \tid ->
    force tid >>= \case
      VAttr attrs ->
        case (M.lookup "key" attrs, M.lookup "value" attrs) of
          (Just tkey, Just tval) ->
            force tkey >>= \case
              VPrim (PString key) -> pure (key, tval)
              _ -> throwError "builtins.listToAttrs: key was not a string"
          _ -> throwError "builtins.listToAttrs: attr set did not contain key and value attributes"
      _ -> throwError "builtins.listToAttrs: list had a non-attribute set"
  pure $ VAttr $ M.fromList pairs
bListToAttrs _ = throwError "builtins.listToAttrs: argument was not a list"
