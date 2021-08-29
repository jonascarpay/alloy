{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}

module Eval (runEvalExpr, runEvalFile, runEvalString) where

import Control.Monad.Except
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Text qualified as T
import Eval.Builtins (builtins, forceExpect)
import Eval.Eval
import Eval.Lib
import Eval.Types
import Expr
import Parser.Parser
import System.FilePath qualified as FP

runEvalExpr :: Expr -> IO (Either String NF)
runEvalExpr expr = runEval $ flip unEval (whnf expr)

runEvalFile :: FilePath -> IO (Either String NF)
runEvalFile fp = runEval $ evalFile fp

runEvalString :: FilePath -> ByteString -> IO (Either String NF)
runEvalString fp bs = runEval $ evalString fp bs

runEval :: (Thunk -> EvalBase WHNF) -> IO (Either String NF)
runEval m = do
  runExceptT $
    flip evalStateT 0 $
      flip runReaderT mempty $
        unEvalBase $ do
          loadBuiltins >>= m >>= deepseq
  where
    deepseq :: WHNF -> EvalBase NF
    deepseq = fmap NF . traverse (force >=> deepseq)

evalString :: FilePath -> ByteString -> Thunk -> EvalBase WHNF
evalString fp bs tnk = do
  path <- liftIO $ mkAbsPath fp
  case parse bs path of
    Left err -> throwError err
    Right expr -> unEval tnk (whnf expr)

evalFile :: FilePath -> Thunk -> EvalBase WHNF
evalFile fp tnk = do
  bs <- liftIO $ BS.readFile fp
  evalString fp bs tnk

loadBuiltins :: EvalBase Thunk
loadBuiltins = do
  tnk <- defer $ throwError "uninitialized builtins"
  v <- mkBuiltins (builtins <> M.singleton "import" (vImport tnk))
  overrideThunk tnk $ Right v
  pure tnk
  where
    deepRefer :: NF -> EvalBase Thunk
    deepRefer = traverse deepRefer . unNF >=> refer
    mkBuiltins :: Map Symbol (Value NF) -> EvalBase WHNF
    mkBuiltins m = VAttr <$> traverse (deepRefer . NF) m
    vImport :: Thunk -> Value f
    vImport tnk = VClosure $
      forceExpect "builtins.import" "a valid path string" $ \case
        VString str | fp <- T.unpack str, FP.isValid fp -> Just $ evalFile fp tnk
        _ -> Nothing
