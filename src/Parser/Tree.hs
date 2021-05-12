{-# LANGUAGE DeriveTraversable #-}

module Parser.Tree where

import Data.ByteString (ByteString)
import Data.Sequence (Seq)

type Name = ByteString

data Binding
  = Binding Name [Name] PTree
  | Inherit [Name]
  | InheritFrom PTree [Name]
  deriving (Eq, Show)

data Atom
  = AInt Int
  | ADouble Double
  | ABool Bool
  | AString ByteString
  deriving (Eq, Show)

data PTree
  = Let [Binding] PTree
  | Var Name
  | App PTree PTree
  | Lam Name PTree
  | Atom Atom
  | List (Seq PTree)
  deriving (Eq, Show)
