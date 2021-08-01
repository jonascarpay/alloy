{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RTTests where

import Control.Monad
import Data.Either
import Debug.Pretty.Simple
import Debug.Trace
import Eval.Types
import Print
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Focus (focus)
import Test.Tasty.HUnit
import TestLib
import Text.RawString.QQ

-- Standalone function, no deps
saFunc :: HasCallStack => String -> String -> TestTree
saFunc nm = funcWithNDeps nm 0

funcWithNDeps :: HasCallStack => String -> Int -> String -> TestTree
funcWithNDeps name exp prog = testCase name $ do
  val <- assertParse prog >>= assertEval
  let throw err = assertFailure $ unlines [err, show (printNF val)]
  case val of
    NF (VFunc (Deps close open) call) -> do
      unless (isRight call) $ throw "unresolved temporary function id"
      unless (null open) $ throw "Function with dangling temporary functions"
      -- let calls = toListOf (traverse . funCalls) fn
      -- unless (all (`M.member` fn) calls) $
      --   throw "dangling unresolved call in a dependency"
      let got = length close - 1
      unless (exp == got) $ throw $ unwords ["expected", show exp, "dependencies, got", show got]
    _ -> throw "Value was not a function"

pending :: TestTree -> TestTree
pending = expectFailBecause "Pending"

negative :: TestTree -> TestTree
negative = expectFailBecause "Negative test"

-- TODO
-- argument length error test
-- Label scoping test

rtTests :: TestTree
rtTests =
  testGroup
    "rt"
    [ testGroup
        "trivial"
        [ saFunc "break" "[] -> builtins.types.int here@{ break here 0; }",
          saFunc "continue" "[] -> builtins.types.int here@{ continue here; }",
          saFunc "empty break" "[] -> builtins.types.void here@{ break here; }",
          saFunc "declaration" "with builtins.types; [] -> void { var i: int = 0; }",
          saFunc "assignment" "with builtins.types; [] -> void { var i: int = 0; i = 9; }",
          saFunc "expression statement" "with builtins.types; [] -> void { 3; builtins.void }",
          saFunc "assignment with lhs expression" "with builtins.types; [] -> void { var i: int = 0; (x: x) i = 9; }",
          saFunc "with" "with builtins.types; [] -> int here@{ break here 0; }",
          saFunc "terminator expression" "[] -> builtins.types.int { 9 }",
          saFunc "bodyless function" "[] -> builtins.types.int 9",
          saFunc "id 1" "with builtins.types; [x : int] -> int here@{ break here x; }",
          saFunc "id 2" "with builtins.types; [x : int] -> int { x }",
          saFunc "id 3" "with builtins.types; [x : int] -> int x",
          saFunc "bodyless function (void)" "[] -> builtins.types.void builtins.void",
          saFunc
            "simple conditional"
            [r| with builtins.types;
                [] -> int {
                  if true then 9 else 10
                }
            |],
          funcWithNDeps
            "conditionals can get eliminated at compile time"
            1
            [r| with builtins.types;
                [] -> int here@{
                  if true then {
                    break here ([] -> int 3)[];
                  } else {
                    break here ([] -> int 4)[];
                  }
                }
            |],
          funcWithNDeps
            "conditionals sometimes don't get eliminated at comptime"
            2
            [r| with builtins.types;
                [] -> int here@{
                  var b : bool = true;
                  if b then {
                    break here ([] -> int 3)[];
                  } else {
                    break here ([] -> int 4)[];
                  }
                }
            |]
        ],
      testGroup
        "recusion"
        [ saFunc "simple infinite recursion" "self@[] -> builtins.types.int { self[] }",
          funcWithNDeps
            "mutual recursion"
            1
            [r| with builtins.types;
              top@[] -> int
                let sub = [] -> int { top[] };
                 in { sub[] }
          |],
          funcWithNDeps
            "recursive case as argument"
            1
            [r| with builtins.types;
              let f = rec: [] -> int { rec[] };
               in self@[] -> int { f self [] }
          |]
        ],
      testGroup
        "deduplication"
        [ funcWithNDeps
            "simplest deduplication"
            2
            [r| with builtins.types;
              let
                a = [] -> int c[];
                b = [] -> int c[];
                c = [] -> int 4;
              in [] -> int a[] + b[]
            |],
          funcWithNDeps
            "deduplication with bindings"
            1
            [r| with builtins.types;
              let
                a = [] -> int { var x: int = 4; x };
                b = [] -> int { var x: int = 4; x };
              in [] -> int { a[] + b[] }
            |],
          funcWithNDeps
            "deduplication behind names"
            1
            [r| with builtins.types;
                let
                  a = [] -> int { var va: int = 4; va };
                  b = [] -> int { var vb: int = 4; vb };
                in [] -> int { a[] + b[] }
            |],
          pending $
            funcWithNDeps
              "deduplication behind bodyless function"
              2
              [r| with builtins.types;
              let
                a = [] -> int  c[] ;
                b = [] -> int { c[] };
                c = [] -> int { 4 };
              in [] -> int { a[] + b[] }
            |],
          funcWithNDeps
            "identical functions"
            2
            [r| with builtins.types;
              let f = n: rec: [] -> int { rec[] + n };
               in self@[] -> int {
                 f 1 self [];
                 f 2 self [];
                 f 1 self [];
               }
            |],
          funcWithNDeps
            "self-similar recursion"
            4
            [r| with builtins.types;
              let f = rec: [] -> int { rec[] };
               in self@[] -> int {
                 self [];
                 f self [];
                 f (f self) [];
                 f (f (f self)) [];
                 f (f (f (f self))) [];
               }
            |]
        ],
      testGroup
        "labels"
        [ saFunc "named blocks parse" "[] -> builtins.types.void { lbl@{ }; }",
          saFunc "labeled function body" "with builtins.types; [] -> int lbl@{ 9 }",
          saFunc
            "labeled break as return"
            [r| with builtins.types;
                [] -> int lbl@{
                  var x : int = 4;
                  break lbl x;
                }
            |],
          negative $
            saFunc
              "labeled break as return (negative)"
              [r| with builtins.types;
                  [] -> void lbl@{
                    var x : int = 4;
                    break lbl x;
                  }
              |],
          negative $
            saFunc
              "labeled break as return (negative 2)"
              "[] -> builtins.types.int lbl@{ break lbl; }",
          saFunc
            "label expression"
            [r| with builtins.types;
                [] -> int lbl@{
                  var x : int = 4;
                  break ((x: x) lbl) x;
                }
            |],
          negative $
            saFunc
              "break as return (negative)"
              [r| with builtins.types;
                  [] -> void {
                    var x : int = 4;
                    x
                  }
              |],
          negative $
            saFunc
              "break as return (negative 2)"
              "[] -> builtins.types.int lbl@{ break lbl; }",
          saFunc
            "nested break return"
            [r| with builtins.types;
                [] -> int lbl@{ {
                  var x : int = 4;
                  break lbl x;
                } }
            |],
          saFunc
            "nested break return"
            [r| with builtins.types;
                [] -> int lbl@{ {
                  var x : int = 4;
                  break lbl x;
                } }
            |],
          negative $
            saFunc
              "nested break return (negative)"
              [r| with builtins.types;
                  [] -> void lbl@{ {
                    var x : int = 4;
                    break lbl x;
                  }; }
              |]
        ],
      testGroup
        "scoping"
        [ expectFailBecause "runtime variables cannot escape their scope" $
            funcWithNDeps
              "runtime variables scoping"
              1
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    ( let y = x;
                       in [] -> int { y }
                    )[];
                  }
              |],
          funcWithNDeps
            "proper argument shadowing"
            1
            [r| with builtins;
                with types;
                [] -> int {
                  var x = 3;
                  var y = ([x: int] -> int { return x; })[x];
                  return x;
                }
            |]
        ],
      testGroup
        "types"
        [ negative $
            saFunc
              "simple declaration type mismatch"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var z: double = x;
                  }
              |],
          negative $
            saFunc
              "simple assignment type mismatch"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var z: double = 4;
                    z = x;
                  }
              |],
          saFunc
            "typeOf forward expression"
            [r| with builtins.types;
                [] -> int {
                  var x = 4;
                  var y: builtins.typeOf x = x;
                  return x;
                }
            |],
          saFunc
            "comparator arithop maintains type"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 4;
                  var z: builtins.typeOf x = x + 3;
                }
            |],
          negative $
            saFunc
              "comparator arithop maintains type (negative)"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var z: bool = x + 3;
                  }
              |],
          saFunc
            "comparator binop resolves to bool"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 4;
                  var z: bool = (x == 3);
                  3
                }
            |],
          negative $
            saFunc
              "comparator binop resolves to bool (negative)"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var z: builtins.typeOf x = (x == 3);
                  }
              |],
          saFunc
            "branches of conditional must match"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 4;
                  var y: int = 4;
                  var z = if true then x else y;
                }
            |],
          negative $
            saFunc
              "branches of conditional must match (negative)"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var y: double = 4;
                    var z = if true then x else y;
                  }
              |],
          saFunc
            "conditional must be bool"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 4;
                  var y: bool = true;
                  var z = if y then x else x;
                }
            |],
          negative $
            saFunc
              "conditional must be bool (negative)"
              [r| with builtins.types;
                  [] -> int {
                    var x: int = 4;
                    var y: int = 4;
                    var z = if y then x else x;
                  }
              |],
          funcWithNDeps
            "argument parallel type inference"
            1
            [r| with builtins; with types;
                let id = t: [a: t] -> t { return a; };
                in [] -> void {
                  var y = 1;
                  var x: typeOf y = id int [y];
                }
            |],
          funcWithNDeps
            "argument parallel type inference"
            1
            [r| with builtins; with types;
                let id = t: [a: t] -> t { return a; };
                in [] -> void {
                  var y = 1;
                  var x: typeOf y = id int [{y + 2}];
                }
            |],
          expectFailBecause "Cannot construct void for nums" $
            saFunc
              "numerical void"
              "with builtins.types; [] -> void { return 0; }",
          saFunc
            "struct field type inference"
            [r| with builtins.types;
                let
                  v = builtins.struct {x: int};
                in [] -> void {
                  var x: v = {x: {0}};
                }
            |],
          negative $
            funcWithNDeps
              "function return type unification"
              1
              [r| with builtins.types;
                  let f = [] -> int { return 3; };
                  in [] -> int { var o: void = f[]; }
              |],
          negative $
            saFunc
              "unify types of blockExprs"
              [r| with builtins.types;
                  [] -> int {
                    var b: double = {var res: int = 0; res};
                  }
              |],
          negative $
            saFunc
              "unify types of blockExprs"
              [r| with builtins.types;
                  let
                    zeroExpr = {var res: int = 0; res };
                  in [] -> int {
                    var a: int = zeroExpr; # this should succeed
                    var b: double = zeroExpr;
                  }
              |],
          saFunc
            "block types are not evaluated"
            [r| with builtins.types;
                let
                  zeroExpr = {var res = 0; res };
                in [] -> int {
                  var a: int = zeroExpr;
                  var b: double = zeroExpr;
                }
            |],
          saFunc
            "closure type propagation"
            [r| with builtins.types;
                [] -> int {
                  var a: bool = true;
                  var x: int = (c: {if c then 3 else 4}) a;
                }|],
          negative $
            saFunc
              "closure type propagation (negative)"
              [r| with builtins.types;
                  [] -> int {
                    var a: int = 3;
                    var x: int = (c: {if c then 3 else 4}) a;
                  }
              |],
          saFunc
            "weird matchType bug (with annotation)"
            [r| with builtins; with types;
                [] -> int {
                  var x : int = 12;
                  var z
                    : matchType { int: builtins.types.int } (typeOf x)
                    = 234;
                  return x;
                } |],
          saFunc
            "weird matchType bug (without annotation)"
            [r| with builtins; with types;
                [] -> int {
                  var x = 12;
                  var z
                    : matchType { int: builtins.types.int } (typeOf x)
                    = 234;
                  return x;
                } |]
        ],
      testGroup
        "structs"
        [ saFunc
            "simple struct literal"
            [r| with builtins.types;
                let str = builtins.struct { a: int, b: double };
                in [] -> void {
                  var x: str = {a: 3, b: 2};
                }
            |],
          negative $
            saFunc
              "struct member type error"
              [r| with builtins.types;
                  let str = builtins.struct { a: bool };
                  in [] -> void {
                    var x: str = {a: 2};
                  }
              |],
          negative $
            saFunc
              "missing struct members"
              [r| with builtins.types;
                  let str = builtins.struct { a: int, b: int };
                  in [] -> void {
                    var x: str = {a: 2};
                  }
              |],
          saFunc
            "non-literal struct expression"
            [r| with builtins.types;
                let str = builtins.struct { a: int, b: double };
                in [] -> void {
                  var y: int = 2;
                  var x: str = {a: y, b: 2};
                }
            |],
          saFunc
            "struct fields are _runtime_ expressions"
            [r| with builtins.types;
                let
                  v = builtins.struct {x: int};
                in [] -> void {
                  var x: v = {x: 0};
                  # var y: v = {x: x.x};
                }
            |],
          saFunc
            "simple RT field accessor"
            [r| with builtins.types;
                [] -> int {
                  var v: builtins.struct {x: int} = { x: 0 };
                  return v.x;
                }
            |],
          saFunc
            "nested RT field accessor"
            [r| with builtins.types;
                let
                  nestedStruct = builtins.struct {x: int};
                  str = builtins.struct {nest: nestedStruct};
                in [] -> int {
                  var v: str = { nest: {x: 0} };
                  return v.nest.x;
                }
            |]
        ],
      testGroup
        "examples"
        [ saFunc
            "half-inlined while loop"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 3;
                    (cond: loop@{
                      if cond then {break loop;} else {dummy@{ x = x + 1; }; continue loop;}; # TODO dummy
                    }) {x < 3};
                }
            |],
          saFunc
            "inline while loop"
            [r| with builtins.types;
                [] -> int {
                  var x: int = 3;
                  loop@{
                    if {x < 3} then {break loop;} else {
                      x = x + 1;
                      continue loop;
                    };
                  };
                }
            |],
          saFunc
            "while loop"
            [r| with builtins.types;
                let while = cond: body: loop@{
                      if cond then {break loop;} else {body; continue loop;};
                    };
                in [] -> int {
                  var x: int = 3;
                  while {x < 3} dummy@{ # TODO dummy
                    x = x + 1;
                  };
                }
            |]
        ]
    ]
