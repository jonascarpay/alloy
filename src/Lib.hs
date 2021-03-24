{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

module Lib where

import Data.Bifunctor
import Evaluate
import Expr
import Parse
import Prettyprinter
import Print
import System.Console.Haskeline
import Text.Megaparsec as MP

-- | Block values aren't typechecked, only functions.
-- This makes sense because they occur during evaluation, and it doesn't make sense to typecheck them every time you encounter one.
-- When we present a block as the result of an evaluation to the user however, we do actually want to report when it contains
-- type errors.
-- So, this function special-cases block values to typecheck them before printing.
-- TODO: maybe not allow printing untypechecked blocks
evalCheckInfo :: Expr -> Either (Doc ann) (Doc ann)
evalCheckInfo expr = evalInfo expr >>= first f . checkAndPrint
  where
    checkAndPrint :: Value -> Either String (Doc ann)
    -- checkAndPrint (Fix (VBlock env b)) = uncurry (ppTypedBlock env) <$> typecheckBlock env b
    checkAndPrint v = pure $ ppVal v
    f err =
      vcat
        [ "Encountered error during type checking:",
          indent 2 $ pretty err,
          "AST:",
          indent 2 $ ppExpr expr
        ]

evalInfo :: Expr -> Either (Doc ann) Value
evalInfo expr = first f (eval expr)
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
            Right expr ->
              outputStrLn
                (either show show (evalCheckInfo expr))
                >> loop
