{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE UndecidableInstances #-}

module Lib where

import Control.Lens
import Control.Monad.Except
import Control.Monad.RWS
import Data.Map (Map)
import Data.Map qualified as M
import Data.Void
import System.Console.Haskeline
import Text.Megaparsec as MP
import Text.Megaparsec.Char as MP
import Text.Megaparsec.Char.Lexer qualified as Lex

type Name = String

data ExprF f
  = Var Name
  | App f f
  | Lam Name f
  | Lit Int
  | Add f f
  deriving (Eq, Show)

data ValueF val
  = VInt Int
  | VClosure Name Expr (Map Name val)
  deriving (Functor, Foldable, Traversable)

--TODO Closure Args (p -> m r)

instance Show (ValueF val) where
  show (VInt n) = show n
  show VClosure {} = "<<closure>>"

newtype Fix f = Fix {unFix :: f (Fix f)}

type Expr = Fix ExprF

type Value = Fix ValueF

type ThunkID = Int

-- TODO Thunk m v = Deferred (m v) | Computed v
data Thunk = Deferred Expr | Computed (ValueF ThunkID)

type Lazy = RWST (Map Name ThunkID) () (Int, Map ThunkID Thunk) (Either String)

eval :: Expr -> Either String Value
eval (Fix eRoot) = runLazy $ step eRoot >>= deepEval
  where
    runLazy :: Lazy a -> Either String a
    runLazy m = fst <$> evalRWST m mempty (0, mempty)
    deepEval :: ValueF ThunkID -> Lazy Value
    deepEval v = Fix <$> traverse (force >=> deepEval) v
    step :: ExprF Expr -> Lazy (ValueF ThunkID)
    step (Lit n) = pure $ VInt n
    step (App (Fix f) x) = do
      tx <- defer x
      step f >>= \case
        (VClosure arg (Fix body) env) -> local (const $ M.insert arg tx env) (step body)
        _ -> throwError "Calling a non-function"
    step (Var x) =
      asks (M.lookup x) >>= \case
        Nothing -> throwError "Unbound variable"
        Just tid -> force tid
    step (Lam arg body) = VClosure arg body <$> ask
    step (Add (Fix a) (Fix b)) = do
      step a >>= \case
        VInt va ->
          step b >>= \case
            VInt vb -> pure (VInt (va + vb))
            _ -> throwError "Adding a non-integer"
        _ -> throwError "Adding a non-integer"
    defer :: Expr -> Lazy ThunkID
    defer expr = state $ \(n, m) -> (n, (n + 1, M.insert n (Deferred expr) m))
    force :: ThunkID -> Lazy (ValueF ThunkID)
    force tid =
      gets (M.lookup tid . snd) >>= \case
        Just (Deferred (Fix expr)) -> do
          v <- step expr
          _2 . at tid .= Just (Computed v)
          pure v
        Just (Computed x) -> pure x
        Nothing -> throwError "Looking up invalid thunk?"

type Parser = ParsecT Void String Identity

pExpr :: Parser Expr
pExpr = foldl1 (\a b -> Fix (App a b)) <$> some pTerm
  where
    pTerm = choice [parens pExpr, pLam, pLit, pVar]
    lexeme :: Parser a -> Parser a
    lexeme p = p <* MP.space
    lchunk = void . lexeme . chunk
    pName :: Parser String
    pName = lexeme $ takeWhile1P (Just "identifier") (`elem` ['a' .. 'z'])
    parens :: Parser p -> Parser p
    parens p = lchunk "(" *> p <* lchunk ")"
    pLam = do
      void $ lchunk "\\"
      xs <- some pName
      lchunk "."
      body <- pExpr
      pure $ foldr (\a b -> Fix (Lam a b)) body xs
    pLit = lexeme $ Fix . Lit <$> Lex.signed (pure ()) Lex.decimal
    pVar = Fix . Var <$> pName

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
