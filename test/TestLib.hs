{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module TestLib where

import Data.ByteString.Char8 qualified as BS8
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

assertValueEq :: HasCallStack => NF -> NF -> Assertion
assertValueEq exp got
  | exp == got = pure ()
  | otherwise = assertEqual "value mismatch" (printNF exp) (printNF got) -- TODO obviously a hack

-- assertFunc :: HasCallStack => Value -> IO (Dependencies, GUID)
-- assertFunc (Fix (VFunc deps (Right guid))) = pure (deps, guid)
-- assertFunc _ = assertFailure "Value was not a function"
