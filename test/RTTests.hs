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
import Test.Hspec
import TestLib
import Text.RawString.QQ

-- Standalone function, no deps
saFunc :: HasCallStack => String -> String -> Spec
saFunc nm = funcWithNDeps nm 0

xsaFunc :: HasCallStack => String -> String -> Spec
xsaFunc nm = markPending . saFunc nm

markPending :: Spec -> Spec
markPending = before_ pending

funcWithNDeps :: HasCallStack => String -> Int -> String -> Spec
funcWithNDeps name exp prog = it name $ do
  val <- assertParse prog >>= assertEval
  let throw err = expectationFailure $ unlines [err, show (printNF val)]
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

xfuncWithNDeps :: HasCallStack => String -> Int -> String -> Spec
xfuncWithNDeps name exp prog = markPending $ funcWithNDeps name exp prog

-- TODO
-- argument length error test
-- Label scoping test

{-# ANN rtTests ("hlint: ignore Redundant $" :: String) #-}
rtTests :: Spec
rtTests = describe "rt" $ do
  describe "trivial" $ do
    saFunc "break" "[] -> builtins.types.int here@{ break here 0; }"
    saFunc "continue" "[] -> builtins.types.int here@{ continue here; }"
    saFunc "empty break" "[] -> builtins.types.void here@{ break here; }"
    saFunc "declaration" "with builtins.types; [] -> void { var i: int = 0; }"
    saFunc "assignment" "with builtins.types; [] -> void { var i: int = 0; i = 9; }"
    saFunc "expression statement" "with builtins.types; [] -> void { 3; builtins.void }"
    saFunc "assignment with lhs expression" "with builtins.types; [] -> void { var i: int = 0; (x: x) i = 9; }"
    saFunc "with" "with builtins.types; [] -> int here@{ break here 0; }"
    saFunc "terminator expression" "[] -> builtins.types.int { 9 }"
    saFunc "bodyless function" "[] -> builtins.types.int 9"
    saFunc "id 1" "with builtins.types; [x : int] -> int here@{ break here x; }"
    saFunc "id 2" "with builtins.types; [x : int] -> int { x }"
    saFunc "id 3" "with builtins.types; [x : int] -> int x"
    saFunc "bodyless function (void)" "[] -> builtins.types.void builtins.void"
    saFunc "simple conditional" $
      [r| with builtins.types;
          [] -> int {
            if true then 9 else 10
          }
      |]
    funcWithNDeps "conditionals can get eliminated at compile time" 1 $
      [r| with builtins.types;
          [] -> int here@{
            if true then {
              break here ([] -> int 3)[];
            } else {
              break here ([] -> int 4)[];
            }
          }
      |]
    funcWithNDeps "conditionals sometimes don't get eliminated at comptime" 2 $
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
  describe "recusion" $ do
    saFunc "simple infinite recursion" "self@[] -> builtins.types.int { self[] }"
    funcWithNDeps "mutual recursion" 1 $
      [r| with builtins.types;
          top@[] -> int
            let sub = [] -> int { top[] };
             in { sub[] }
      |]
    funcWithNDeps "recursive case as argument" 1 $
      [r| with builtins.types;
          let f = rec: [] -> int { rec[] };
           in self@[] -> int { f self [] }
      |]
  describe "deduplication" $ do
    funcWithNDeps "simplest deduplication" 2 $
      [r| with builtins.types;
          let
            a = [] -> int c[];
            b = [] -> int c[];
            c = [] -> int 4;
          in [] -> int a[] + b[]
        |]
    funcWithNDeps "deduplication with bindings" 1 $
      [r| with builtins.types;
          let
            a = [] -> int { var x: int = 4; x };
            b = [] -> int { var x: int = 4; x };
          in [] -> int { a[] + b[] }
      |]
    funcWithNDeps "deduplication behind names" 1 $
      [r| with builtins.types;
          let
            a = [] -> int { var va: int = 4; va };
            b = [] -> int { var vb: int = 4; vb };
          in
            [] -> int { a[] + b[] }
      |]
    xfuncWithNDeps "deduplication behind bodyless function" 2 $
      [r| with builtins.types;
          let
            a = [] -> int  c[] ;
            b = [] -> int { c[] };
            c = [] -> int { 4 };
          in [] -> int { a[] + b[] }
      |]
    funcWithNDeps "identical functions" 2 $
      [r| with builtins.types;
          let f = n: rec: [] -> int { rec[] + n };
           in self@[] -> int {
             f 1 self [];
             f 2 self [];
             f 1 self []
           }
      |]
    funcWithNDeps "self-similar recursion" 4 $
      [r| with builtins.types;
          let f = rec: [] -> int { rec[] };
           in self@[] -> int {
             self [];
             f self [];
             f (f self) [];
             f (f (f self)) [];
             f (f (f (f self))) []
           }
      |]
  describe "labels" $ do
    saFunc "named blocks parse" "[] -> builtins.types.void { lbl@{ }; }"
    saFunc "labeled function body" "with builtins.types; [] -> int lbl@{ 9 }"
    saFunc "labeled break as return" $
      [r| with builtins.types;
            [] -> int lbl@{
              var x : int = 4;
              break lbl x;
            }
        |]
    nocompile "labeled break as return (negative)" $
      [r| with builtins.types;
          [] -> void lbl@{
            var x : int = 4;
            break lbl x;
          }
      |]
    nocompile "labeled break as return (negative 2)" "[] -> builtins.types.int lbl@{ break lbl; }"
    saFunc "label expression" $
      [r| with builtins.types;
          [] -> int lbl@{
            var x : int = 4;
            break ((x: x) lbl) x;
          }
      |]
    nocompile "break as return (negative)" $
      [r| with builtins.types;
          [] -> void {
            var x : int = 4;
            x
          }
      |]
    nocompile "break as return (negative 2)" "[] -> builtins.types.int lbl@{ break lbl; }"
    saFunc "nested break return" $
      [r| with builtins.types;
          [] -> int lbl@{ {
            var x : int = 4;
            break lbl x;
          } }
      |]
    saFunc "nested break return" $
      [r| with builtins.types;
          [] -> int lbl@{ {
            var x : int = 4;
            break lbl x;
          } }
      |]
    nocompile "nested break return (negative)" $
      [r| with builtins.types;
          [] -> void lbl@{ {
            var x : int = 4;
            break lbl x;
          }; }
      |]
  describe "scoping" $ do
    nocompile "runtime variables scoping" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            ( let y = x;
               in [] -> int { y }
            )[];
          }
      |]
    funcWithNDeps "proper argument shadowing" 1 $
      [r| with builtins;
          with types;
          [] -> int {
            var x = 3;
            var y = ([x: int] -> int { x })[x];
            x
          }
      |]
  describe "types" $ do
    nocompile "simple declaration type mismatch" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            var z: double = x;
          }
      |]
    nocompile "simple assignment type mismatch" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            var z: double = 4;
            z = x;
          }
      |]
    nocompile "comparator arithop maintains type (negative)" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            var z: bool = x + 3;
          }
      |]
    saFunc "comparator binop resolves to bool" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            var z: bool = (x == 3);
            3
          }
      |]
    saFunc "branches of conditional must match" $
      [r| with builtins.types;
          [] -> void {
            var x: int = 4;
            var y: int = 4;
            var z = if {true} then x else y;
          }
      |]
    nocompile "branches of conditional must match (negative)" $
      [r| with builtins.types;
          [] -> coid {
            var x: int = 4;
            var y: double = 4;
            var z = if {true} then x else y;
          }
      |]
    saFunc "conditional must be bool" $
      [r| with builtins.types;
          [] -> int {
            var x: int = 4;
            var y: bool = true;
            var z = if y then x else x;
            9
          }
      |]
    nocompile "conditional must be bool (negative)" $
      [r| with builtins.types;
          [] -> void {
            var x: int = 4;
            var y: int = 4;
            var z = if y then x else x;
          }
      |]
    nocompile "numerical void" "with builtins.types; [] -> void { 0 }"
    saFunc "tuple field type inference" $
      [r| let
            inherit (builtins.types) tuple int void;
            v = tuple [int];
          in [] -> void {
            var x: v = [0];
          }
      |]
    nocompile "function return type unification" $
      [r| with builtins.types;
          let f = [] -> int { 3 };
          in [] -> void { var o: void = f[]; }
      |]
    nocompile "unify types of blockExprs" $
      [r| with builtins.types;
          [] -> int {
            var b: double = {var res: int = 0; res};
          }
      |]
    nocompile "unify types of blockExprs" $
      [r| with builtins.types;
          let
            zeroExpr = {var res: int = 0; res };
          in [] -> void {
            var a: int = zeroExpr; # this should succeed
            var b: double = zeroExpr;
          }
      |]
    saFunc "block types are polymorphic" $
      [r| with builtins.types;
          let
            zeroExpr = {var res = 0; res };
          in [] -> void {
            var a: int = zeroExpr;
            var b: double = zeroExpr;
          }
      |]
    saFunc "closure type propagation" $
      [r| with builtins.types;
          [] -> void {
            var a: bool = true;
            var x: int = (c: {if c then 3 else 4}) a;
          }
      |]
    nocompile "closure type propagation (negative)" $
      [r| with builtins.types;
          [] -> int {
            var a: int = 3;
            var x: int = (c: {if c then 3 else 4}) a;
          }
      |]
    saFunc "block expression type" $
      [r| with builtins.types;
          [] -> int {
            var sum: int = 0;
            lbl@{ sum = 2; };
            sum
          }
      |]
  describe "tuples" $ do
    saFunc "simple tuple literal" $
      [r| let inherit (builtins.types) tuple void int double;
              tup = tuple [int, double];
          in [] -> void {
            var x: tup = [2, 3];
          }
      |]
    nocompile "struct member type error" $
      [r| let inherit (builtins.types) tuple bool void;
              tup = tuple [bool];
          in [] -> void {
            var x: tup = [2];
          }
      |]
    nocompile "missing struct members" $
      [r| let inherit (builtins.types) tuple int void;
              tup = tuple [int, int];
          in [] -> void {
            var x: tup = [2];
          }
      |]
    saFunc "non-literal struct expression" $
      [r| let inherit (builtins.types) tuple int double void;
              tup = tuple [int, double];
          in [] -> void {
            var y: int = 2;
            var x: tup = [y, 3];
          }
      |]
    saFunc "simple RT field accessor" $
      [r| let inherit (builtins.types) int tuple;
          in [] -> int {
            var v: tuple [int] = [0];
            v.0
          }
        |]
    saFunc "tuple fields are runtime expressions" $
      [r| let inherit (builtins.types) tuple void int;
              tup = tuple [int];
          in [] -> void {
            var x: tup = [2];
            var y: tup = [x.0];
          }
      |]
    saFunc "tuple accessors are runtime expressions" $
      [r| let inherit (builtins.types) tuple void int;
              tup = tuple [int];
          in [] -> void {
            var x: tup = [2];
            var y: tup = [x.0];
          }
      |]
    saFunc "nested RT field accessor" $
      [r| let
            inherit (builtins.types) tuple int;
            nest = tuple [int];
            tup = tuple [nest];
          in [] -> int {
            var v: tup = [[0]];
            v.0.0
          }
      |]
    saFunc "by-value array" $
      [r| let
            inherit (builtins.types) tuple int;
            arr n t = tuple (n * [t]);
          in [] -> int {
            var v: arr 100 int = [4] * 100;
            v.99
          }
      |]
    saFunc "tuple assignment" $
      [r| let
            inherit (builtins.types) tuple int void;
            nest = tuple [int];
            tup = tuple [nest];
          in [] -> void {
            var v: tup = [[0]];
            v.0.0 = 3;
          }
      |]
    funcWithNDeps "tuple rvalue accessor" 1 $
      [r| let
            inherit (builtins.types) tuple int void;
            tup = [] -> (tuple [int]) [2];
          in [] -> int ((tup[]).0)
      |]
  describe "pointers" $ do
    saFunc "pointer dereference" $
      [r| let
            inherit (builtins.types) int ptr;
          in [] -> int {
            var q: int = 0;
            var pq: ptr int = &q;
            ^pq = 2;
            q
          }
      |]
    nocompile "invalid pointer dereference" $
      [r| let
            inherit (builtins.types) int double ptr;
          in [] -> int {
            var q: int = 0;
            var pq: ptr double = &q;
            ^pq = 2;
            q
          }
      |]
  describe "examples" $ do
    -- TODO
    -- There's an interesting point about the type system for these loops.  Say
    -- you want somthing like
    -- {
    --   var q: int = 0;
    --   i <- for 0 q;
    --   q = q+ i;
    -- }
    -- This does not make it clear whether it takes q as a reference or a
    -- value, and observably changes its behavior based on that.  The larger
    -- issue is just that q implicitly fulfills the roles of lvalue, lvalue
    -- reference, and rvalue.  Conversions could be made mandatory or more
    -- explicit, or the type system at least tells you what things are cast to.
    saFunc "half-inlined while loop" $
      [r| with builtins.types;
          [] -> void {
            var x: int = 3;
              (cond: loop@{
                if cond then {break loop;} else {dummy@{ x = x + 1; }; continue loop;}; # TODO dummy
              }) {x < 3};
          }
      |]
    saFunc "inline while loop" $
      [r| with builtins.types;
          [] -> void {
            var x: int = 3;
            loop@{
              if {x < 3} then {break loop;} else {
                x = x + 1;
                continue loop;
              };
            };
          }
      |]
    saFunc "while loop" $
      [r| with builtins.types;
          let while cond body = loop@{ if cond then {break loop;} else {body; continue loop;}; };
          in [] -> void {
            var x: int = 3;
            while {x < 3} dummy@{ # TODO dummy
              x = x + 1;
            };
          }
      |]
    saFunc "continuation notation" $
      [r| with builtins.types;
          let range lo hi body = {
                var i: int = lo;
                loop@{
                  if i < hi then {body i;} else {break loop;};
                  i = i + 1;
                  continue loop;
                };
              };
              zeroTo = range 0;
           in [] -> int {
             var sum = 0;
             { i <- zeroTo 10;
               j <- zeroTo i;
               sum = sum + j;
             };
             sum
           }
      |]
