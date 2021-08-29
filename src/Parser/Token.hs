module Parser.Token where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char (toLower)
import Data.Text (Text)
import Data.Text.Encoding qualified as TE

-- TODO
-- unicode
-- λ

data SourcePos = SourcePos
  { lineNumber :: {-# UNPACK #-} !Int,
    columnNumber :: {-# UNPACK #-} !Int,
    byteOffset :: {-# UNPACK #-} !Int
  }
  deriving (Eq)

instance Show SourcePos where
  show (SourcePos l c _) = "L" <> show l <> ":" <> show c

data Token
  = -- Compile time keywords
    Let
  | In
  | With
  | Inherit
  | -- Common keywords
    If
  | Then
  | Else
  | TTrue
  | TFalse
  | -- Runtime keywords
    Break
  | Continue
  | Var
  | -- Operators
    Add
  | Sub
  | Mul
  | Div
  | Eq
  | Neq
  | Lt
  | Gt
  | Leq
  | Geq
  | Dot
  | RArr
  | LArr
  | -- Referencing operators
    Ampersand
  | Caret
  | -- Punctuation
    Semicolon
  | Colon
  | Comma
  | Assign
  | At
  | -- Delimiters
    LParen
  | RParen
  | LBrace
  | RBrace
  | LBrack
  | RBrack
  | -- Other things
    Ident !Text
  | String !Text
  | Num Int
  | Path !Text -- TODO FilePath? We perform operations from `filepath` on it before turning into string
  deriving (Eq, Show)

tok :: Token -> a -> Token
tok = const

{-# ANN module ("hlint: ignore Use camelCase" :: String) #-}

tok_num :: ByteString -> Token
tok_num bs = Num (parse bs)
  where
    parse = BS.foldl' f 0
    f acc b = acc * 10 + fromIntegral b - 48

tok_string :: ByteString -> Token
tok_string = String . TE.decodeUtf8 . BS.unsafeInit . BS.unsafeTail

tok_ident :: ByteString -> Token
tok_ident = Ident . TE.decodeUtf8

-- Drops the leading "./"
tok_path :: ByteString -> Token
tok_path = Path . TE.decodeUtf8 . BS.drop 2

descrToken :: Token -> String
descrToken (Ident x) = "identifier " <> show x
descrToken Add = "+"
descrToken Mul = "*"
descrToken Div = "/"
descrToken Sub = "-"
descrToken Gt = ">"
descrToken Lt = "<"
descrToken At = "@"
descrToken Assign = "="
descrToken LParen = "("
descrToken RParen = ")"
descrToken LBrace = "{"
descrToken RBrace = "}"
descrToken LBrack = "["
descrToken RBrack = "]"
descrToken x = toLower <$> show x
