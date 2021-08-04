{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.Builtins (builtins) where

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

-- TODO
-- matchvalue
-- import

{-# ANN module ("hlint: ignore Use >=>" :: String) #-}

builtins :: NF
builtins =
  NF . VAttr . fmap NF $
    M.fromList
      [ ("nine", VPrim $ PInt 9),
        ("length", vLength),
        ("index", vIndex),
        ("listToAttrs", vListToAttrs),
        ("types", vTypes),
        ("matchType", vMatchType),
        ("void", vVoid)
      ]

-- TODO maybe do something prismy instead?
{-# INLINE forceExpect #-}
forceExpect :: String -> String -> (WHNF -> Maybe (EvalBase r)) -> Thunk -> EvalBase r
forceExpect prefix ex k tnk = do
  val <- force tnk
  case k val of
    Just r -> r
    Nothing -> throwError $ prefix <> ": Expected " <> ex <> ", but got " <> describeValue val

vLength :: Value f
vLength = VClosure $ \tList ->
  force tList >>= \case
    VList l -> pure . VPrim . PInt $ length l
    VString s -> pure . VPrim . PInt $ BS.length s
    val -> throwError $ "builtins.length: Cannot get the length of " <> describeValue val

vIndex :: Value f
vIndex = VClosure . expect "a list, string, or attribute set" $ \case
  VList l -> Just . pure . VClosure . expect "an integer" $ \case
    VPrim (PInt ix) -> Just $ case Seq.lookup ix l of
      Nothing -> throwError $ "builtins.index: List index " <> show ix <> " is out of bounds"
      Just t -> force t
    _ -> Nothing
  VString bs -> Just . pure . VClosure . expect "an integer" $ \case
    VPrim (PInt ix) -> Just $ case indexMaybe bs ix of
      Nothing -> throwError $ "builtins.index: List index " <> show ix <> " is out of bounds"
      Just t -> pure . VString $ BS.singleton t
    _ -> Nothing
  VAttr attrs -> Just . pure . VClosure . expect "a string" $ \case
    VString str -> Just $ case M.lookup (BSS.toShort str) attrs of
      Nothing -> throwError $ "builtins.index: Attribute set does not contain field " <> show str
      Just t -> force t
    _ -> Nothing
  _ -> Nothing
  where
    expect = forceExpect "builtins.index"

vListToAttrs :: Value f
vListToAttrs = VClosure . expect "a list" $ \case
  VList l -> Just . fmap (VAttr . M.fromList . toList) . forM l $
    expect "an attribute set" $ \case
      VAttr attrs -> Just $ case M.lookup "key" attrs of
        Just tKey -> flip (expect "a string") tKey $ \case
          VString key -> Just $ case M.lookup "value" attrs of
            Just value -> pure (BSS.toShort key, value)
            Nothing -> throwError "builtins.listToAttrs: Attribute set did not contain field \"value\""
          _ -> Nothing
        Nothing -> throwError "builtins.listToAttrs: Attribute set did not contain field \"key\""
      _ -> Nothing
  _ -> Nothing
  where
    expect = forceExpect "builtins.listToAttrs"

vTypes :: Value NF
vTypes =
  VAttr $
    NF
      <$> M.fromList
        [ ("int", VType TInt),
          ("double", VType TDouble),
          ("bool", VType TBool),
          ("void", VType TVoid),
          ("struct", VClosure mkStruct)
        ]
  where
    mkStruct = forceExpect "builtins.types.struct" "an attribute set" $ \case
      VAttr m -> Just $ do
        m' <- traverse (force >=> ensureType) m
        pure $ VType $ TStruct m'
      _ -> Nothing

vMatchType :: Value f
vMatchType = VClosure $
  expect "an attribute set" $ \case
    VAttr m -> Just $
      pure $
        VClosure $
          expect "a type" $ \case
            VType t ->
              Just $
                case t of
                  TInt -> lookupDefault m "int"
                  TDouble -> lookupDefault m "double"
                  TBool -> lookupDefault m "bool"
                  TVoid -> lookupDefault m "void"
                  TStruct fields ->
                    lookupDefault m "struct" >>= \case
                      VClosure k -> do
                        fields' <- traverse (refer . VType) fields
                        refer (VAttr fields') >>= k
                      val -> throwError $ "builtins.matchType: Matcher for struct fields was not a closure but " <> describeValue val
            _ -> Nothing
    _ -> Nothing
  where
    expect = forceExpect "builtins.matchType"
    {-# INLINE lookupDefault #-}
    lookupDefault :: Map Name Thunk -> Name -> EvalBase WHNF
    lookupDefault m name
      | Just r <- M.lookup name m = force r
      | Just r <- M.lookup "default" m = force r
      | otherwise = throwError $ "builtins.matchType: Attribute set does not contain matcher " <> show name

vVoid :: Value f
vVoid = VPrim PVoid
