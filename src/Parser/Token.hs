module Parser.Token where

import Data.ByteString (ByteString)
import Data.ByteString qualified as BS
import Data.ByteString.Unsafe qualified as BS
import Data.Char (toLower)

data SourcePos = SourcePos
  { lineNumber :: Int,
    columnNumber :: Int
  }
  deriving (Eq)

instance Show SourcePos where
  show (SourcePos l c) = "L" <> show (l + 1) <> ":" <> show (c + 1)

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
  | Ident ByteString
  | String ByteString
  | Num Int
  deriving (Eq, Show)

tok :: Token -> ByteString -> Token
tok = const

tok_num :: ByteString -> Token
tok_num bs = Num (parse bs)
  where
    parse = BS.foldl' f 0
    f acc b = acc * 10 + fromIntegral b - 48

tok_string :: ByteString -> Token
tok_string = String . BS.unsafeInit . BS.unsafeTail

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
