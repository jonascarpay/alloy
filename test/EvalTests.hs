{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module EvalTests where

import Eval.Types
import Expr
import Test.Hspec
import TestLib
import Text.RawString.QQ

is9 :: HasCallStack => String -> String -> Spec
is9 name prog =
  it name $
    assertParse prog
      >>= assertEval
      >>= assertValueEq (NF $ VPrim $ PInt 9)

{-# ANN evalTests ("hlint: ignore Redundant $" :: String) #-}
evalTests :: Spec
evalTests = describe "eval" $ do
  is9 "const" "9"
  is9 "add" "4+5"
  is9 "mul" "3*3"
  is9 "sub" "13-4"
  is9 "div" "27/3"
  is9 "precedence" "3 * 5 - 6"
  is9 "precedence 2" "3 * 13 - 5 * 6"
  is9 "associativity" "15 - 5 - 1"
  is9 "id" "(x: x) 9"
  is9 "fst" "(x: y: x) 9 11"
  is9 "snd" "(x: y: y) 11 9"
  is9 "snd (shadow)" "(x: x: x) 11 9"
  is9 "simple attribute set" "{foo = 9;}.foo"
  is9 "simple let binding" "let x = 9; in x"
  is9 "let with nonrecursive local reference" "let y = 9; x = y; in x"
  is9 "nested attrs" "{foo = {bar = 9;};}.foo.bar"
  is9 "let binding with nested attrs" "let attrs = {foo = {bar = 9;};}; in attrs.foo.bar"
  is9 "reference in attr binding" "(x: {a = x;}.a) 9"
  is9 "not sure what to call it but it used to fail" "let id = x: x; x = 9; in id x"
  is9 "id id id id id" "let id = x: x; x = 9; in id id id id x"
  is9 "scoping test" "(id: x: (id id) (id x)) (x: x) 9"
  is9 "let scoping test" "let id = x: x; x = 9; in (id id) (id x)"
  is9 "laziness test" "let diverge = (x: x x) (x: x x); in 9"
  is9 "builtin nine" "builtins.nine"
  is9 "inherit from" $
    [r| let attr = { a = builtins.undefined; b = 9; };
            inherit (attr) b;
        in b
    |]
  is9 "inherit from expression" $
    [r| let inherit ({ a = builtins.undefined; b = 9; }) b;
        in b
    |]
  is9 "attrs inherit from" $
    [r| let attrs = { a = 9; };
        in { inherit (attrs) a; }.a
    |]
  is9 "lazy attr inheritance test" $
    [r| let diverge = builtins.undefined;
            x = 9;
         in { inherit diverge;
              inherit x;
            }.x
    |]
  is9 "laziness ignores undefined" "with builtins; let x = error \"undefined\"; y = 9; in y"
  is9 "y combinator" $
    [r| let y = f: (x: f (x x)) (x: (f (x x)));
            attr = y (self: {
              three = 3;
              nine = self.three * self.three; });
           in attr.nine
    |]
  is9 "forward let reference" "let a = b; b = 9; in a"
  is9 "recursive let reference" "let attr = { foo = 9; bar = attr.foo; }; in attr.bar"
  is9 "line comments" $
    [r| let a = 9; # comment"
         in a
    |]
  is9 "with-expression" "with {a = 9;}; a"
  is9 "simple if true" "if true then 9 else 10"
  is9 "simple if false" "if false then 10 else 9"
  is9 "lazy if true" "if true then 9 else builtins.undefined"
  is9 "lazy if false" "if false then builtins.undefined else 9"
  is9 "nested if true" "if true then if true then 9 else 10 else 10"
  is9 "nested if false" "if false then 10 else if false then 10 else 9"
  is9 "if comparison" "if 2 + 2 < 5 then 9 else 10"
  describe "matchType" $ do
    is9 "matchType" "builtins.matchType { int = 9; default = builtins.undefined; } builtins.types.int"
    is9 "matchType default" "builtins.matchType { default = 9; } builtins.types.void"
    is9 "matchType pointer" $
      [r| let inherit (builtins.types) int ptr;
              match = builtins.matchType {
                int = 7;
                ptr x = (match x) + 1;
              };
          in match (ptr (ptr int))
      |]
    is9 "matchType tuple" $
      [r| with builtins;
          with types;
          let t = tuple [ double, int ];
          in matchType {
               tuple = ms: matchType  {
                   int = 9;
                   default = undefined;
                 } ms.1;
               default = undefined;
             } t
      |]
  describe "indexing" $ do
    is9 "attrset string lookup" [r| { nine = 9; }."nine" |]
    is9 "list indexing" "[1, 3, 9, 27].2"
    is9 "index expression" [r| { nine = 9; }.("ni" + "ne") |]
    is9 "list index expression" "[1, 3, 9, 27].(1+1)"
    is9 "string indexing" [r| if "139".2 == "9" then 9 else builtins.undefined |]
  describe "unicode" $ do
    is9 "unicode strings" [r| if "α" + "ω" == "αω" then 9 else 10 |]
    is9 "unicode identifiers" [r| (珍穴子🤯️: 渦巻: 珍穴子🤯️) 9 "💩" |]
  is9 "list length" "builtins.length [1, 2, 3, 4, 5, 6, 7, 8, 9]"
  is9 "string length" "builtins.length \"123456789\""
  is9 "list concat" "([] + [9] + []).0"
  is9 "right replication" "([9] * 9).8"
  is9 "left replication" "(9 * [9]).8"
  is9 "listToAttrs" "(builtins.listToAttrs [{ key = \"nine\"; value = 9; }]).nine"
  is9 "haskell function syntax 1" "let id x = x; in id 9"
  is9 "haskell function syntax 2" "let const x y = x; in const 9 10"
  is9 "simple string comparison" "if \"test\" == \"test\" then 9 else 8"
  is9 "concatenated string comparison" "if \"te\" + \"st\" == \"test\" then 9 else 8"
  is9 "newlines in string parsing" $
    [r| let str = "test
                ";
      in 9
    |]
  is9 "paths" "let path = ./foo/bar.baz; in if path == \"/testPath/foo/bar.baz\" then 9 else path"
