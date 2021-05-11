{-# LANGUAGE LambdaCase #-}

module Parser.Parser (parse) where

import Control.Applicative.Combinators
import Data.ByteString (ByteString)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Vector (Vector)
import Data.Vector qualified as V
import Parser.Lexer qualified as Lex
import Parser.Parsec qualified as P
import Parser.Token (Token)
import Parser.Token qualified as T
import Parser.Tree

type Parser = P.Parser (Maybe Token) (Set String)

pExpr :: Parser PTree
pExpr =
  choice
    [ pLam,
      pApp
    ]

pApp :: Parser PTree
pApp = do
  h <- pTerm
  foldr (flip App) h <$> many pTerm

pTerm :: Parser PTree
pTerm =
  choice
    [ pVar
    ]
  where
    pVar = Var <$> pIdent

pIdent :: Parser Name
pIdent = expect "identifier" $ \case
  (T.Ident name) -> Just name
  _ -> Nothing

pLam :: Parser PTree
pLam = do
  arg <- pIdent
  token T.Colon
  Lam arg <$> pExpr

token :: Token -> Parser ()
token tk = expect (T.showToken tk) $
  \tk' -> if tk == tk' then Just () else Nothing

expect :: String -> (Token -> Maybe a) -> Parser a
expect msg f =
  P.throwCC $ \throw ->
    let err = throw $ Set.singleton msg
     in P.token >>= maybe err (maybe err pure . f)

eof :: Parser ()
eof = P.throwCC $ \throw ->
  P.token >>= \case
    Nothing -> pure ()
    _ -> throw $ Set.singleton "end of input"

tokenize :: ByteString -> Vector Token
tokenize bs = case Lex.lexer bs of
  Left err -> error (show err)
  Right tokens -> V.fromList (fst <$> tokens)

parse :: ByteString -> PTree
parse bs = case P.runParser (tokens V.!?) (pExpr <* eof) of
  Left (_, err) -> error (show err)
  Right a -> a
  where
    tokens = tokenize bs
