{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Builtins where

-- TODO Builtins are in this module only because matchType -> reduce -> step -> Expr

import Control.Monad.Except
import Control.Monad.RWS
import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
import Data.Map qualified as M
import Data.Sequence qualified as Seq
import Eval
import Expr
import Lens.Micro.Platform
import Parser.Parser
import System.FilePath

type Builtin1 = ExpressionEnv -> LazyValue -> Eval LazyValue

type Builtin2 = LazyValue -> LazyValue -> Eval LazyValue

bError :: Builtin1
bError _ (VPrim (PString msg)) = throwError $ "error: " <> BS8.unpack msg
bError _ _ = throwError "builtins.error was passed a non-string"

attrNames :: Builtin1
attrNames _ (VAttr attrs) = do
  names <- traverse (deferVal . VPrim . PString) (M.keys attrs)
  pure $ VList $ Seq.fromList names
attrNames _ _ = throwError "attrNames on non-list"

-- TODO combine with normal field accessor
bLookup :: Builtin2
bLookup (VAttr attrs) (VPrim (PString str)) =
  case M.lookup str attrs of
    Nothing -> throwError "field doesn't exist"
    Just v -> force v -- TODO this should be lazier
bLookup (VAttr _) _ = throwError "second argument to stringField was not a string"
bLookup _ _ = throwError "first argument to stringField was not an attr set"

bIndex :: Builtin2
bIndex (VList xs) (VPrim (PInt i)) =
  case Seq.lookup i xs of
    Nothing -> throwError $ "builtins.index: index " <> show i <> " out of bounds"
    Just x -> force x
bIndex (VList _) _ = throwError "builtins.index called with not an integer"
bIndex _ _ = throwError "builtins.index called with not a list"

bLength :: Builtin1
bLength _ (VList xs) = pure $ VPrim $ PInt $ Seq.length xs
bLength _ (VPrim (PString s)) = pure $ VPrim $ PInt $ BS.length s
bLength _ _ = throwError "builtins.length called with not a list or string"

bStruct :: Builtin1
bStruct _ (VAttr m) = do
  let forceType tid = do
        force tid >>= \case
          (VType t) -> pure t
          _ -> throwError "Struct member was not a type expression"
  types <- traverse forceType m
  pure $ VType $ TStruct types
bStruct _ _ = throwError "Making a struct typedef from something that's not an attrset"

-- -- TODO this could be lazier if VClosure' were lazier
-- bImport :: (Expr -> Eval a) -> LazyValue -> Eval a
-- bImport f (VPrim (PString rel)) = do
--   file <- view envFile
--   let file' = takeDirectory file </> BS8.unpack rel
--   input <- liftIO $ BS.readFile file'
--   case parse input of
--     Left err -> throwError err
--     Right expr -> local (envFile .~ file') $ f expr
-- bImport _ _ = throwError "import needs a filepath"

bListToAttrs :: Builtin1
bListToAttrs _ (VList xs) = fmap (VAttr . M.fromList) $
  forM (toList xs) $ \tid ->
    force tid >>= \case
      VAttr attrs ->
        case (M.lookup "key" attrs, M.lookup "value" attrs) of
          (Just tkey, Just tval) ->
            force tkey >>= \case
              VPrim (PString key) -> pure (key, tval)
              _ -> throwError "builtins.listToAttrs: key was not a string"
          _ -> throwError "builtins.listToAttrs: attr set did not contain key and value attributes"
      _ -> throwError "builtins.listToAttrs: list had a non-attribute set"
bListToAttrs _ _ = throwError "builtins.listToAttrs: argument was not a list"
