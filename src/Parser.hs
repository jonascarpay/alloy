module Parser where

import Data.ByteString (ByteString)
import Parser.Lexer qualified as L
import Parser.Parser qualified as P

parse :: ByteString -> P.AST
parse bs = P.calc tokens
  where
    tokens = case L.lexer bs of
      Left err -> error (show err)
      Right t -> fst <$> t
