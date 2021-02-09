{-# LANGUAGE LambdaCase #-}

module Lib where

import Eval
import Expr
import Parse
import System.Console.Haskeline
import Text.Megaparsec as MP

repl :: IO ()
repl = runInputT defaultSettings loop
  where
    loop =
      getInputLine "Vandelay Industries> " >>= \case
        Nothing -> outputStrLn "You're my favorite customer"
        Just str -> case parse pExpr "" str of
          Left err -> outputStrLn (errorBundlePretty err) >> loop
          Right expr -> case eval expr of
            Left err -> outputStrLn err >> loop
            Right val -> outputStrLn (show $ unFix val) >> loop
