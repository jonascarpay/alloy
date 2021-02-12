{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Eval
import Parse
import Prettyprinter
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
            Left err -> do
              outputStrLn . show $
                vcat
                  [ "Encountered error during evaluation:",
                    indent 2 $ pretty err,
                    "AST:",
                    indent 2 $ ppExpr expr
                  ]
              loop
            Right val -> outputStrLn (show . ppVal $ val) >> loop

evalFile :: FilePath -> IO ()
evalFile fp = do
  input <- readFile fp
  expr <- either (fail . errorBundlePretty) pure (parse pToplevel fp input)
  either fail (print . ppVal) (eval expr)

evalFile1 = evalFile "./syntax.ayy"
