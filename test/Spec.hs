{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

import Control.Monad
import Data.Bifunctor
import Data.Map qualified as M
import Eval
import Expr
import Lib
import Parse
import Prettyprinter
import Print
import Test.Tasty
import Test.Tasty.Focus
import Test.Tasty.HUnit
import Text.Megaparsec qualified as MP
import Text.RawString.QQ

main :: IO ()
main =
  defaultMain $
    testGroup
      "alloy-test"
      [ evalTests,
        testSyntax
      ]

assertParse :: String -> IO Expr
assertParse str = do
  case MP.parse pToplevel "" str of
    Left err -> assertFailure $ MP.errorBundlePretty err
    Right res -> pure res

testSyntax :: TestTree
testSyntax = testCase "syntax.ayy" $ do
  f <- readFile "syntax.ayy"
  assertParse f >>= \expr ->
    case evalCheckInfo expr of
      Left doc -> assertFailure (show doc)
      Right doc -> assertFailure (show doc)

evalTests :: TestTree
evalTests =
  withFocus . testGroup "eval" $
    [ is9 "const" "9",
      is9 "add" "4+5",
      is9 "mul" "3*3",
      is9 "sub" "13-4",
      is9 "precedence" "3 * 5 - 6",
      is9 "precedence 2" "3 * 13 - 5 * 6",
      is9 "associativity" "15 - 5 - 1",
      is9 "id" "(x: x) 9",
      is9 "fst" "(x: y: x) 9 11",
      is9 "snd" "(x: y: y) 11 9",
      is9 "simple attribute set" "{foo: 9}.foo",
      is9 "simple let binding" "let x = 9; in x",
      is9
        "let with local reference"
        [r| let y = 9;
                  x = y;
               in x
        |],
      is9 "nested attrs" "{foo: {bar: 9}}.foo.bar",
      is9 "let binding with nested attrs" "let attrs = {foo: {bar: 9}}; in attrs.foo.bar",
      is9 "reference in attr binding" "(x: {a: x}.a) 9",
      is9
        "not sure what to call it but it used to fail"
        [r|let id = x: x;
                 x = 9;
              in id x
        |],
      is9
        "id id id id id"
        [r| let id = x: x;
                  x = 9;
               in id id id id x
        |],
      is9
        "scoping test"
        "(id: x: (id id) (id x)) (x: x) 9",
      is9
        "let scoping test"
        [r| let id = x: x;
                  x = 9;
               in (id id) (id x)
        |],
      is9
        "laziness test"
        "let diverge = (x: x x) (x: x x); in 9",
      is9
        "inherit from"
        [r|
          let attr = { a: builtins.undefined, b: 9 };
              inherit (attr) b;
           in b
        |],
      is9
        "lazy attr inheritance test"
        [r| let diverge = builtins.undefined;
                  x = 9;
               in { inherit diverge,
                    inherit x
                  }.x
        |],
      is9 "simple builtin" "builtins.nine",
      is9
        "laziness ignores undefined"
        [r| let x = builtins.undefined;
                y = builtins.nine;
             in y
        |],
      is9
        "y combinator"
        [r| let y = f: (x: f (x x)) (x: (f (x x)));
                attr = y (self: {
                  three: 3,
                  nine: self.three * self.three});
               in attr.nine
        |],
      is9
        "forward let reference"
        [r| let a = b;
                b = 9;
             in a
          |],
      is9
        "recursive let reference"
        [r| let attr = { foo: 9, bar: attr.foo };
               in attr.bar
          |],
      is9
        "line comments"
        [r| let a = 9; # comment"
               in a
        |],
      is9
        "with-expression"
        "with builtins; nine"
    ]

is9 :: String -> String -> TestTree
is9 name prog = assertEval name prog (Fix $ VPrim $ PInt 9)

valueCompare :: Value -> Value -> Either (Doc ann) ()
valueCompare (Fix (VPrim pa)) (Fix (VPrim pb)) | pa == pb = pure ()
valueCompare (Fix (VAttr na)) (Fix (VAttr nb)) | M.keys na == M.keys nb = zipWithM_ valueCompare (M.elems na) (M.elems nb)
valueCompare a b = Left $ hsep ["mismatch between", ppVal a, "and", ppVal b]

assertEval :: String -> String -> Value -> TestTree
assertEval name program expect =
  testCase name $
    assertParse program >>= \expr ->
      case evalInfo expr of
        Left err -> assertFailure $ show err
        Right got -> either (assertFailure . show) pure $ valueCompare expect got
