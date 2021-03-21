{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib where

import Control.Monad
import Data.Map qualified as M
import Eval
import Expr
import Lib
import Parse
import Prettyprinter
import Print
import Program
import Test.Tasty.HUnit
import Text.Megaparsec qualified as MP

assertFile :: FilePath -> IO String
assertFile = readFile

assertParse :: HasCallStack => String -> IO Expr
assertParse str = do
  case MP.parse pToplevel "" str of
    Left err -> assertFailure $ MP.errorBundlePretty err
    Right res -> pure res

assertEval :: Expr -> IO Value
assertEval expr =
  case evalInfo expr of
    Left err -> assertFailure $ show err
    Right val -> pure val

assertValueEq :: Value -> Value -> Assertion
assertValueEq exp got = either (assertFailure . show) pure $ go exp got
  where
    go :: Value -> Value -> Either (Doc ann) ()
    go (Fix (VPrim pa)) (Fix (VPrim pb)) | pa == pb = pure ()
    go (Fix (VAttr na)) (Fix (VAttr nb)) | M.keys na == M.keys nb = zipWithM_ go (M.elems na) (M.elems nb)
    go a b = Left $ hsep ["mismatch between", ppVal a, "and", ppVal b]

assertFunc :: Value -> IO (Dependencies, GUID)
assertFunc (Fix (VFunc deps (Right guid))) = pure (deps, guid)
assertFunc _ = assertFailure "Value was not a function"
