{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Eval.Builtins (builtins) where

import Control.Monad.Except
import Data.Foldable (toList)
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Eval.Lib
import Eval.Types
import Expr

-- TODO
-- import

{-# ANN module ("hlint: ignore Use >=>" :: String) #-}

builtins :: Map Symbol (Value NF)
builtins =
  M.fromList
    [ ("length", vLength),
      ("listToAttrs", vListToAttrs),
      ("types", vTypes),
      ("matchType", vMatchType),
      ("known", vKnown),
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
    VString s -> pure . VPrim . PInt $ T.length s
    val -> throwError $ "builtins.length: Cannot get the length of " <> describeValue val

vListToAttrs :: Value f
vListToAttrs = VClosure . expect "a list" $ \case
  VList l -> Just . fmap (VAttr . M.fromList . toList) . forM l $
    expect "an attribute set" $ \case
      VAttr attrs -> Just $ case M.lookup "key" attrs of
        Just tKey -> flip (expect "a string") tKey $ \case
          VString key -> Just $ case M.lookup "value" attrs of
            Just value -> pure (key, value)
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
        [ ("int", VType $ Type TInt),
          ("double", VType $ Type TDouble),
          ("bool", VType $ Type TBool),
          ("void", VType $ Type TVoid),
          ("tuple", VClosure mkTuple),
          ("array", mkArray),
          ("ptr", VClosure mkPtr)
        ]
  where
    mkTuple = forceExpect "builtins.types.tuple" "a list of member types" $ \case
      VList m -> Just $ VType . Type . TTuple <$> traverse (force >=> ensureType) m
      _ -> Nothing
    mkArray =
      let expect str tn k = forceExpect "builtins.types.tuple" str k tn
       in VClosure $ \tn ->
            expect "the array size" tn $ \case
              VPrim (PInt n) -> Just $
                pure $
                  VClosure $ \tt ->
                    expect "the array element type" tt $ \case
                      VType t -> Just $ pure $ VType $ Type $ TArray n t
                      _ -> Nothing
              _ -> Nothing
    mkPtr = forceExpect "builtins.types.ptr" "a type" $ \case
      VType t -> Just $ pure $ VType $ Type $ TPtr t
      _ -> Nothing

vKnown :: Value f
vKnown =
  VClosure $
    force >=> \case
      (VRTPlace _ _) -> pure (VPrim $ PBool False)
      (VRTValue _ _) -> pure (VPrim $ PBool False)
      _ -> pure (VPrim $ PBool True)

-- TODO
-- Instead of pattern matching on the matchers and applying arguments in the
-- case of array/tuple/pointer, it would be better to have some sort of
-- general-purpose apply function.  Maybe we then just have good enough stack
-- traces to be able to infer what's wrong from context.
vMatchType :: Value f
vMatchType = VClosure $
  expect "an attribute set" $ \case
    VAttr m -> Just $
      pure $
        VClosure $
          expect "a type" $ \case
            VType (Type t) ->
              Just $
                case t of
                  TInt -> lookupDefault m "int"
                  TDouble -> lookupDefault m "double"
                  TBool -> lookupDefault m "bool"
                  TVoid -> lookupDefault m "void"
                  TTuple members ->
                    lookupDefault m "tuple" >>= \case
                      VClosure k -> do
                        members' <- traverse (refer . VType) members
                        refer (VList members') >>= k
                      val -> throwError $ "builtins.matchType: Matcher for tuple fields was not a closure but " <> describeValue val
                  TPtr t ->
                    lookupDefault m "ptr" >>= \case
                      VClosure k ->
                        refer (VType t) >>= k
                      val -> throwError $ "builtins.matchType: Matcher for pointer type was not a closure but " <> describeValue val
                  TArray n t ->
                    lookupDefault m "array" >>= \case
                      VClosure k ->
                        refer (VPrim $ PInt n) >>= k >>= \case
                          VClosure k' ->
                            refer (VType t) >>= k'
                          _ -> throwError "builtins.matchType: Matcher for array was not a function taking two arguments"
                      _ -> throwError "builtins.matchType: Matcher for array was not a function taking two arguments"
            _ -> Nothing
    _ -> Nothing
  where
    expect = forceExpect "builtins.matchType"
    {-# INLINE lookupDefault #-}
    lookupDefault :: Map Symbol Thunk -> Symbol -> EvalBase WHNF
    lookupDefault m name
      | Just r <- M.lookup name m = force r
      | Just r <- M.lookup "default" m = force r
      | otherwise = throwError $ "builtins.matchType: Attribute set does not contain matcher " <> show name

vVoid :: Value f
vVoid = VPrim PVoid
