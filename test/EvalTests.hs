{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EvalTests where

import Eval
import Expr
import Test.Tasty
import Test.Tasty.Focus
import Test.Tasty.HUnit
import TestLib
import Text.RawString.QQ

is9 :: HasCallStack => String -> String -> TestTree
is9 name prog =
  testCase name $
    assertParse prog
      >>= assertEval
      >>= assertValueEq (Fix $ VPrim $ PInt 9)

evalTests :: TestTree
evalTests =
  testGroup
    "eval"
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
      is9 "let with nonrecursive local reference" "let y = 9; x = y; in x",
      is9 "nested attrs" "{foo: {bar: 9}}.foo.bar",
      is9 "let binding with nested attrs" "let attrs = {foo: {bar: 9}}; in attrs.foo.bar",
      is9 "reference in attr binding" "(x: {a: x}.a) 9",
      is9 "not sure what to call it but it used to fail" "let id = x: x; x = 9; in id x",
      is9 "id id id id id" "let id = x: x; x = 9; in id id id id x",
      is9 "scoping test" "(id: x: (id id) (id x)) (x: x) 9",
      is9 "let scoping test" "let id = x: x; x = 9; in (id id) (id x)",
      is9 "laziness test" "let diverge = (x: x x) (x: x x); in 9",
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
      is9 "laziness ignores undefined" "with builtins; let x = undefined; y = nine; in y",
      is9
        "y combinator"
        [r| let y = f: (x: f (x x)) (x: (f (x x)));
                attr = y (self: {
                  three: 3,
                  nine: self.three * self.three});
               in attr.nine
        |],
      is9 "forward let reference" "let a = b; b = 9; in a",
      is9 "recursive let reference" "let attr = { foo: 9, bar: attr.foo }; in attr.bar",
      is9
        "line comments"
        [r| let a = 9; # comment"
             in a
        |],
      is9 "with-expression" "with builtins; nine",
      is9 "simple if true" "if true then 9 else 10",
      is9 "simple if false" "if false then 10 else 9",
      is9 "lazy if true" "if true then 9 else builtins.undefined",
      is9 "lazy if false" "if false then builtins.undefined else 9",
      is9 "nested if true" "if true then if true then 9 else 10 else 10",
      is9 "nested if false" "if false then 10 else if false then 10 else 9",
      is9 "if comparison" "if 2 + 2 < 5 then 9 else 10",
      is9 "typeOf" "if builtins.typeOf {break;} == builtins.types.void then 9 else 8",
      is9 "matchType" "builtins.matchType (builtins.types.int) { int: 9, default: builtins.undefined }",
      is9
        "matchType struct"
        [r| with builtins;
            with types;
            let t = struct { x: double, y: int };
            in matchType t {
              struct: ms: matchType (ms.y) {
                int: 9,
                default: undefined
              },
              default: undefined
            }
        |],
      is9 "matchType default" "builtins.matchType builtins.types.void { default: 9, }"
    ]