{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RTTests where

import Control.Monad
import Data.Either (isRight)
import Data.Map qualified as M
import Eval
import Expr
import Lens.Micro.Platform
import Print
import Program
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.Focus
import Test.Tasty.HUnit
import TestLib
import Text.RawString.QQ

-- Standalone function, no deps
saFunc :: String -> String -> TestTree
saFunc nm = funcWithNDeps nm 0

funcWithNDeps :: String -> Int -> String -> TestTree
funcWithNDeps name exp prog = testCase name $ do
  val <- assertParse prog >>= assertEval
  let throw err = assertFailure $ unlines [err, show (ppVal val)]
  case val of
    (Fix (VFunc (Dependencies fn temp) call)) -> do
      unless (isRight call) $ throw "unresolved temporary function id"
      unless (null temp) $ throw "Function with dangling temporary functions"
      let calls = toListOf (traverse . funCalls) fn
      unless (all (`M.member` fn) calls) $
        throw "dangling unresolved call in a dependency"
      let got = length fn - 1
      unless (exp == got) $ throw $ unwords ["expected", show exp, "dependencies, got", show got]
    _ -> throw "Value was not a function"

pending :: TestTree -> TestTree
pending = expectFailBecause "Pending"

negative :: TestTree -> TestTree
negative = expectFailBecause "Negative test"

-- TODO
-- argument length error test

rtTests :: TestTree
rtTests =
  testGroup
    "rt"
    [ pending $
        saFunc "trivial" "[] -> builtins.types.int { return 0; }",
      pending $
        saFunc "empty function" "with builtins.types; [] -> void { }",
      saFunc "trivial with" "with builtins.types; [] -> int { return 0; }",
      expectFailBecause "Cannot construct void for nums" $
        saFunc "numerical void" "with builtins.types; [] -> void { return 0; }",
      saFunc "simple recursion" "[] -> (builtins.types.int) { return self []; }",
      funcWithNDeps
        "mutual recursion"
        1
        [r| with builtins.types;
            [] -> int
              let top = self;
                  sub = [] -> int { return top[]; };
               in {return sub[];}
        |],
      funcWithNDeps
        "recursive case as argument"
        1
        [r| with builtins.types;
            let f = rec: [] -> int { return rec[]; };
             in [] -> int { return f self []; }
        |],
      funcWithNDeps
        "temporary local functions"
        2
        [r| with builtins.types;
            let f = n: rec: [] -> int { return rec[] + n; };
             in [] -> int {
               return f 1 self [];
               return f 2 self [];
               return f 1 self [];
             }
          |],
      funcWithNDeps
        "deeper recursion"
        4
        [r| with builtins.types;
            let f = rec: [] -> int { return rec[]; };
             in [] -> int {
               return self [];
               return f self [];
               return f (f self) [];
               return f (f (f self)) [];
               return f (f (f (f self))) [];
             }
          |],
      funcWithNDeps
        "simplest deduplication"
        2
        [r| with builtins.types;
            let
              a = [] -> int { c[] };
              b = [] -> int { c[] };
              c = [] -> int { 4 };
            in [] -> int {
              return a[] + b[];
            }
         |],
      funcWithNDeps
        "deduplication with bindings"
        1
        [r| with builtins.types;
            let
              a = [] -> int { var x: int = 4; x };
              b = [] -> int { var x: int = 4; x };
            in [] -> int {
              return a[] + b[];
            }
         |],
      funcWithNDeps
        "semantic deduplication"
        1
        [r| with builtins.types;
            let
              a = [] -> int { var va: int = 4; va };
              b = [] -> int { var vb: int = 4; vb };
            in [] -> int {
              return a[] + b[];
            }
         |],
      saFunc
        "nested return"
        [r| with builtins.types;
            [] -> int { {
              var x : int = 4;
              return x;
            };}
        |],
      negative $
        saFunc
          "nested return (negative)"
          [r| with builtins.types;
              [] -> void { {
                var x : int = 4;
                return x;
              };}
          |],
      saFunc
        "nested return without semicolon"
        "[] -> (builtins.types.int) { { return 4; } }",
      saFunc "named blocks parse" "[] -> (builtins.types.void) { lbl@{ }; }",
      saFunc "labeled function body" "with builtins.types; [] -> int lbl@{ return 3; }",
      saFunc
        "labeled break as return"
        [r| with builtins.types;
            [] -> int lbl@{
              var x : int = 4;
              break @lbl x;
            }
        |],
      negative $
        saFunc
          "labeled break as return (negative)"
          [r| with builtins.types;
              [] -> void lbl@{
                var x : int = 4;
                break @lbl x;
              }
          |],
      negative $
        saFunc
          "labeled break as return (negative 2)"
          "[] -> (builtins.types.int) lbl@{ break @lbl; }",
      saFunc
        "break as return"
        [r| with builtins.types;
            [] -> int {
              var x: int = 4;
              break x;
            }
        |],
      negative $
        saFunc
          "break as return (negative)"
          [r| with builtins.types;
              [] -> void {
                var x : int = 4;
                break x;
              }
          |],
      negative $
        saFunc
          "break as return (negative 2)"
          "[] -> (builtins.types.int) { break; }",
      saFunc
        "nested break return"
        [r| with builtins.types;
            [] -> int lbl@{ {
              var x : int = 4;
              break @lbl x;
            }; }
        |],
      negative $
        saFunc
          "nested break return (negative)"
          [r| with builtins.types;
              [] -> void lbl@{ {
                var x : int = 4;
                break @lbl x;
              }; }
          |],
      expectFailBecause "runtime variables cannot escape their scope" $
        funcWithNDeps
          "runtime variables scoping"
          1
          [r| with builtins.types;
                [] -> int {
                  var x: int = 4;
                  ( let y = x;
                     in [] -> int { return y; }
                  )[];
                }
          |],
      saFunc
        "simple conditional"
        [r| with builtins.types;
            [] -> int {
              if true then {break 2;} else {break 2;}
            }
        |],
      funcWithNDeps
        "conditional evaluates both branches"
        2
        [r| with builtins.types;
            [] -> int {
              if true then {
                break ([] -> int { return 3; })[];
              } else {
                break ([] -> int { return 4; })[];
              };
            }
        |],
      saFunc
        "terminator expression"
        "[] -> (builtins.types.int) { 3 }",
      negative $
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
                return 3;
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
      saFunc
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
        "inline while loop"
        [r| with builtins.types;
            [] -> int {
              var x: int = 3;
              loop@{
                if {x < 3} then {break @loop;} else {
                  {x = x + 1;};
                  continue @loop;
                };
              };
            }
        |],
      -- TODO this defaults to the default type on Evaluate.hs:L212 for some reason???
      saFunc
        "half-inlined while loop"
        [r| with builtins.types;
            [] -> int {
              var x: int = 3;
                (cond: loop@{
                  if cond then {break @loop;} else {{ x = x + 1; }; continue @loop;};
                }) {x < 3};
            }
        |],
      saFunc
        "while loop"
        [r| with builtins.types;
            let while = cond: body: loop@{
                  if cond then {break @loop;} else {body; continue @loop;};
                };
            in [] -> int {
              var x: int = 3;
              while {x < 3} {
                x = x + 1;
              };
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
            } |],
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
      negative $
        funcWithNDeps
          "function return type unification"
          1
          [r| with builtins.types;
            let f = [] -> int { return 3; };
            in [] -> int { var o: void = f[]; }
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
        |],
      saFunc
        "struct field type inference"
        [r| with builtins.types;
            let
              v = builtins.struct {x: int};
            in [] -> void {
              var x: v = {x: {0}};
            }
        |]
    ]
