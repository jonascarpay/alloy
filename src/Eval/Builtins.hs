{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.Builtins (builtins) where

-- TODO Fixpoint, since this can contain builtins.types.int

import Control.Monad.Except
import Data.ByteString qualified as BS
import Data.ByteString.Short qualified as BSS
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Eval.Lib
import Eval.Types
import Expr

{-# ANN builtins ("hlint: ignore" :: String) #-}
builtins :: Map Name (Value a)
builtins =
  M.fromList
    [ ("nine", VPrim $ PInt 9),
      ("length", vLength),
      ("index", vIndex),
      ("listToAttrs", vListToAttrs)
    ]
  where
    vLength = VClosure $ \tList ->
      force tList >>= \case
        VList l -> pure . VPrim . PInt $ length l
        VString s -> pure . VPrim . PInt $ BS.length s
        val -> throwError $ "builtins.length: Cannot get the length of a " <> describeValue val
    vIndex = VClosure $ \tList ->
      force tList >>= \case
        VList l -> pure . VClosure $ \tIx ->
          force tIx >>= \case
            VPrim (PInt ix) -> case Seq.lookup ix l of
              Nothing -> throwError $ "builtins.index: List index " <> show ix <> " is out of bounds"
              Just t -> force t
            val -> throwError $ "builtins.index: Second argument was not an integer but a " <> describeValue val
        VString bs -> pure . VClosure $ \tIx ->
          force tIx >>= \case
            VPrim (PInt ix) -> case indexMaybe bs ix of
              Nothing -> throwError $ "builtins.index: List index " <> show ix <> " is out of bounds"
              Just t -> pure . VString $ BS.singleton t
            val -> throwError $ "builtins.index: Second argument was not an integer but a " <> describeValue val
        VAttr attrs -> pure $
          VClosure $ \tStr ->
            force tStr >>= \case
              VString str -> case M.lookup (BSS.toShort str) attrs of
                Nothing -> throwError $ "builtins.index: Attribute set does not contain field " <> show str
                Just t -> force t
              val -> throwError $ "builtins.index: Second argument was not a string but a " <> describeValue val
        val -> throwError $ "builtins.index: Cannot get index into a " <> describeValue val
    vListToAttrs = VClosure $ \tList ->
      force tList >>= \case
        VList l -> do
          l' <- forM l $ \tAttrs ->
            force tAttrs >>= \case
              VAttr attrs -> case M.lookup "key" attrs of
                Just tKey ->
                  force tKey >>= \case
                    VString key -> case M.lookup "value" attrs of
                      Just value -> pure (BSS.toShort key, value)
                      Nothing -> throwError "builtins.listToAttrs: Attribute set did not contain field \"value\""
                    val -> throwError $ "builtins.listToAttrs: field \"key\" was not a string but a " <> describeValue val
                Nothing -> throwError "builtins.listToAttrs: Attribute set did not contain field \"key\""
              val -> throwError $ "builtins.listToAttrs: List element was not an attribute set but a " <> describeValue val
          pure $ VAttr $ M.fromList $ toList l'
        val -> throwError $ "builtins.listToAttrs: Argument was not a list but a " <> describeValue val
