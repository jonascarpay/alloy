{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes #-}

module RTTests where

import Eval
import Test.Tasty
import Test.Tasty.ExpectedFailure (expectFailBecause)
import Test.Tasty.HUnit
import TestLib
import Text.RawString.QQ

-- Standalone function, no deps
saFunc :: String -> String -> TestTree
saFunc nm = funcWithNDeps nm 0

funcWithNDeps :: String -> Int -> String -> TestTree
funcWithNDeps name n prog = testCase name $ do
  (Dependencies fn temp, _) <- assertParse prog >>= assertEval >>= assertFunc
  assertEqual "" (length temp) 0
  assertEqual "" n (length fn - 1)

pending :: TestTree -> TestTree
pending = expectFailBecause "Pending"

negative :: TestTree -> TestTree
negative = expectFailBecause "Negative test"

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
        expectFailBecause "Cannot construct void from nums" $
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
      pending $
        funcWithNDeps
          "repeated recursion"
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
      pending $
        saFunc "nested return without semicolon" "[] -> (builtins.types.int) { { return 4; } }",
      saFunc "named blocks parse" "[] -> (builtins.types.void) { lbl@{ }; }",
      saFunc "labeled function body" "with builtins.types; [] -> int lbl@{ return 3; }",
      saFunc
        "break as if return"
        [r| with builtins.types;
            [] -> int lbl@{
              var x : int = 4;
              break @lbl x;
            }
        |],
      negative $
        saFunc
          "break as if return (negative)"
          [r| with builtins.types;
              [] -> void lbl@{
                var x : int = 4;
                break @lbl x;
              }
          |],
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
          |]
    ]
