{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib where

import Control.Exception (throwIO)
import Data.ByteString.UTF8 qualified as UTF8
import Data.Map qualified as M
import Eval
import Eval.Types
import Expr
import GHC.Exception (getCallStack)
import GHC.Stack (callStack)
import Parser.Parser
import Print
import Test.HUnit (assertFailure)
import Test.HUnit.Lang (FailureReason (..), HUnitFailure (HUnitFailure))
import Test.Hspec

assertFile :: FilePath -> IO String
assertFile = readFile

assertParse :: HasCallStack => String -> IO Expr
assertParse str = either assertFailure pure $ parse (UTF8.fromString str)

assertEval :: HasCallStack => Expr -> IO NF
assertEval expr = do
  runEval expr >>= \case
    Left err -> assertFailure $ show err
    Right val -> pure val

shallowEq :: HasCallStack => (f -> f -> Bool) -> Value f -> Value f -> Bool
shallowEq f = go
  where
    go VClosure {} _ = False
    go (VRTValue _ a) (VRTValue _ b) = a == b
    go (VRTValue _ _) _ = False
    go (VRTPlace _ _) _ = error "impossible"
    go (VFunc _ a) (VFunc _ b) = a == b
    go (VFunc _ _) _ = False
    go (VType a) (VType b) = a == b
    go (VType _) _ = False
    go (VPrim a) (VPrim b) = a == b
    go (VPrim _) _ = False
    go (VAttr a) (VAttr b) = M.keys a == M.keys b && and (M.intersectionWith f a b)
    go (VAttr _) _ = False
    go (VBlk _) _ = error "impossible"

nfEq :: HasCallStack => NF -> NF -> Bool
nfEq (NF a) (NF b) = shallowEq nfEq a b

nocompile :: HasCallStack => String -> String -> Spec
nocompile name prog = it name $ do
  expr <- assertParse prog
  runEval expr >>= \case
    Right _ -> expectationFailure "Compiled"
    Left _ -> pure ()

assertValueEq :: HasCallStack => NF -> NF -> Expectation
assertValueEq exp got
  | nfEq exp got = pure ()
  | otherwise =
    throwIO $ HUnitFailure (Just loc) $ ExpectedButGot Nothing (UTF8.toString (printNF exp)) (UTF8.toString (printNF got))
  where
    loc = snd $ head $ getCallStack callStack

-- assertFunc :: HasCallStack => Value -> IO (Dependencies, GUID)
-- assertFunc (Fix (VFunc deps (Right guid))) = pure (deps, guid)
-- assertFunc _ = assertFailure "Value was not a function"
