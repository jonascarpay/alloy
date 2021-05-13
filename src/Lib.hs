{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Data.Bifunctor
import Evaluate
import Expr
import Parser.Parser
import Prettyprinter
import Print
import System.Console.Haskeline
import System.Directory

evalInfo :: FilePath -> Expr -> IO (Either (Doc ann) Value)
evalInfo fp expr = (fmap . first) f (eval fp expr)
  where
    f err =
      vcat
        [ "Encountered error during evaluation:",
          indent 2 $ pretty err,
          "AST:",
          indent 2 $ ppExpr expr
        ]

-- repl :: IO ()
-- repl = do
--   cwd <- getCurrentDirectory
--   let loop =
--         getInputLine "> " >>= \case
--           Nothing -> outputStrLn "You're my favorite customer"
--           Just str ->
--             case parse pToplevel "" str of
--               Left err -> outputStrLn (errorBundlePretty err) >> loop
--               Right expr -> do
--                 liftIO (evalInfo cwd expr) >>= either (outputStrLn . show) (outputStrLn . show . ppVal)
--                 loop
--   runInputT defaultSettings {historyFile = Just "~/alloy_repl_hist"} loop
