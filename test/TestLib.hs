{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib where

import Data.ByteString.Char8 qualified as BS8
import Data.Map qualified as M
import Eval
import Eval.Types
import Expr
import Parser.Parser
import Print
import Test.Tasty.HUnit

assertFile :: FilePath -> IO String
assertFile = readFile

assertParse :: HasCallStack => String -> IO Expr
assertParse str = either assertFailure pure $ parse (BS8.pack str)

assertEval :: HasCallStack => Expr -> IO NF
assertEval expr = do
  runEval expr >>= \case
    Left err -> assertFailure $ show err
    Right val -> pure val

shallowEq :: (f -> f -> Bool) -> Value f -> Value f -> Bool
shallowEq f = go
  where
    go VClosure {} _ = False
    go (VRTValue _ a) (VRTValue _ b) = a == b
    go (VRTValue _ _) _ = False
    go (VFunc _ a) (VFunc _ b) = a == b
    go (VFunc _ _) _ = False
    go (VType a) (VType b) = a == b
    go (VType _) _ = False
    go (VPrim a) (VPrim b) = a == b
    go (VPrim _) _ = False
    go (VAttr a) (VAttr b) = M.keys a == M.keys b && and (M.intersectionWith f a b)
    go (VAttr _) _ = False
    go (VVar _) _ = error "impossible"
    go (VBlk _) _ = error "impossible"

nfEq :: NF -> NF -> Bool
nfEq (NF a) (NF b) = shallowEq nfEq a b

assertValueEq :: HasCallStack => NF -> NF -> Assertion
assertValueEq exp got
  | nfEq exp got = pure ()
  | otherwise =
    assertFailure $
      unlines
        [ "value mismatch, expected",
          "  " <> BS8.unpack (printNF exp),
          "but got",
          "  " <> BS8.unpack (printNF got)
        ]

-- assertFunc :: HasCallStack => Value -> IO (Dependencies, GUID)
-- assertFunc (Fix (VFunc deps (Right guid))) = pure (deps, guid)
-- assertFunc _ = assertFailure "Value was not a function"
