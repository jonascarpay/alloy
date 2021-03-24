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
      funcWithNDeps "trivial with" 0 "with builtins.types; [] -> int { return 0; }",
      pending $
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
      -- currently, function scoping is handled by clearing all variables not
      -- bound to thunks (i.e. variables, loop labels) before entering a
      -- function body. The goal is to make sure we can't refer to any runtime things
      -- not defined in the current function.
      -- However, by adding a layer of indirection through a let-binding,
      -- the variable is not actually cleared.
      -- I can think of a number of ways to solve this, but I haven't decided on one yet.
      pending $
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
      funcWithNDeps
        "simple conditional"
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
        "[] -> (builtins.types.int) { 3 }"
    ]
