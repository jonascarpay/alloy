{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Data.Bifunctor
import Evaluate
import Expr
import Parse
import Prettyprinter
import Print
import System.Console.Haskeline
import Text.Megaparsec as MP

evalInfo :: Expr -> IO (Either (Doc ann) Value)
evalInfo expr = (fmap . first) f (eval expr)
  where
    f err =
      vcat
        [ "Encountered error during evaluation:",
          indent 2 $ pretty err,
          "AST:",
          indent 2 $ ppExpr expr
        ]

repl :: IO ()
repl = runInputT defaultSettings {historyFile = Just "~/alloy_repl_hist"} loop
  where
    loop =
      getInputLine "> " >>= \case
        Nothing -> outputStrLn "You're my favorite customer"
        Just str ->
          case parse pToplevel "" str of
            Left err -> outputStrLn (errorBundlePretty err) >> loop
            Right expr -> do
              liftIO (evalInfo expr) >>= either (outputStrLn . show) (outputStrLn . show . ppVal)
              loop
