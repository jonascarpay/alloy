{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Control.Monad.IO.Class
import Data.Bifunctor
import Data.ByteString.Char8 qualified as BS8
import Evaluate
import Expr
import Parser.Parser
import Prettyprinter
import Print
import System.Console.Haskeline

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
repl = do
  let loop =
        getInputLine "> " >>= \case
          Nothing -> outputStrLn "You're my favorite customer"
          Just str ->
            case parse (BS8.pack str) of
              Left err -> outputStrLn err >> loop
              Right expr -> do
                liftIO (evalInfo expr) >>= either (outputStrLn . show) (outputStrLn . show . ppVal)
                loop
  runInputT defaultSettings {historyFile = Just "~/alloy_repl_hist"} loop
