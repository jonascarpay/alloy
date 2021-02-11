{-# LANGUAGE OverloadedStrings #-}

import Control.Monad
import Data.Map qualified as M
import Eval
import Expr
import Parse
import Prettyprinter
import Print
import Test.Tasty
import Test.Tasty.Focus
import Test.Tasty.HUnit
import Text.Megaparsec qualified as MP

main :: IO ()
main = defaultMain $ testGroup "alloy-test" [evalTests]

evalTests :: TestTree
evalTests =
  withFocus . testGroup "eval" $
    fmap
      (uncurry is9)
      [ ("const", "9"),
        ("add", "4+5"),
        ("mul", "3*3"),
        ("sub", "13-4"),
        ("id", "(x: x) 9"),
        ("fst", "(x: y: x) 9 11"),
        ("snd", "(x: y: y) 11 9"),
        ("simple attribute set", "{foo = 9;}.foo"),
        ("simple let binding", "let x = 9; in x"),
        ( "let with local reference",
          "let y = 9; \
          \    x = y; \
          \ in x"
        ),
        ( "nested attrs",
          "{foo = {bar = 9;};}.foo.bar"
        ),
        ( "let binding with nested attrs",
          "let attrs = {foo = {bar = 9;};}; \
          \ in attrs.foo.bar"
        ),
        ("reference in attr binding", "(x: {a = x;}.a) 9"),
        ( "not sure what to call it but it ~fails~ used to fail",
          "let id = x: x; \
          \    x = 9; \
          \ in id x"
        ),
        ( "id id id id id",
          "let id = x: x; \
          \    x = 9; \
          \ in id id id id x"
        ),
        ( "scoping test 1",
          "(id: x: (id id) (id x)) (x: x) 9"
        ),
        ( "laziness test",
          "let diverge = (x: x x) (x: x x); in 9"
        )
      ]

is9 :: String -> String -> TestTree
is9 name prog = assertEval name prog (Fix $ VInt 9)

valueCompare :: Value -> Value -> Either (Doc ann) ()
valueCompare (Fix (VInt na)) (Fix (VInt nb)) | na == nb = pure ()
valueCompare (Fix (VAttr na)) (Fix (VAttr nb)) | M.keys na == M.keys nb = zipWithM_ valueCompare (M.elems na) (M.elems nb)
valueCompare a b = Left $ hsep ["mismatch between", ppVal a, "and", ppVal b]

assertEval :: String -> String -> Value -> TestTree
assertEval name program expect = testCase name $
  case MP.parse pToplevel "" program of
    Left err -> assertFailure $ MP.errorBundlePretty err
    Right expr -> case eval expr of
      Left err -> assertFailure err
      Right got -> either (assertFailure . show) (const $ pure ()) $ valueCompare expect got
