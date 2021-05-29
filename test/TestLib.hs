{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib where

import Control.Monad
import Data.ByteString.Char8 qualified as BS8
import Data.Map qualified as M
import Eval
import Expr
import Lib
import Parser.Parser
import Prettyprinter
import Print
import Program
import Test.Tasty.HUnit

assertFile :: FilePath -> IO String
assertFile = readFile

assertParse :: HasCallStack => String -> IO Expr
assertParse str = either assertFailure pure $ parse (BS8.pack str)

assertEval :: HasCallStack => Expr -> IO Value
assertEval expr = do
  evalInfo expr >>= \case
    Left err -> assertFailure $ show err
    Right val -> pure val

assertValueEq :: HasCallStack => Value -> Value -> Assertion
assertValueEq exp got = either (assertFailure . show) pure $ go exp got
  where
    go :: Value -> Value -> Either (Doc ann) ()
    go (Fix (VPrim pa)) (Fix (VPrim pb)) | pa == pb = pure ()
    go (Fix (VAttr na)) (Fix (VAttr nb)) | M.keys na == M.keys nb = zipWithM_ go (M.elems na) (M.elems nb)
    go a b = Left $ hsep ["mismatch between", ppVal a, "and", ppVal b]

assertFunc :: HasCallStack => Value -> IO (Dependencies, GUID)
assertFunc (Fix (VFunc deps (Right guid))) = pure (deps, guid)
assertFunc _ = assertFailure "Value was not a function"
