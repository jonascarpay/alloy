{-# LANGUAGE LambdaCase #-}

module Lib where

import Eval
import Parse
import Print
import System.Console.Haskeline
import Text.Megaparsec as MP

repl :: IO ()
repl = runInputT defaultSettings {historyFile = Just "~/alloy_repl_hist"} loop
  where
    loop =
      getInputLine "Vandelay Industries> " >>= \case
        Nothing -> outputStrLn "You're my favorite customer"
        Just str -> case parse pToplevel "" str of
          Left err -> outputStrLn (errorBundlePretty err) >> loop
          Right expr -> case eval expr of
            Left err -> outputStrLn err >> outputStrLn (show $ ppExpr expr) >> loop
            Right val -> outputStrLn (show . ppVal $ val) >> loop
