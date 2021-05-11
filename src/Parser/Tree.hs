{-# LANGUAGE DeriveTraversable #-}

module Parser.Tree where

import Data.ByteString (ByteString)

type Name = ByteString

data Binding
  = Binding Name [Name] PTree
  | Inherit [Name]
  | InheritFrom PTree [Name]
  deriving (Eq, Show)

data PTree
  = Let [Binding] PTree
  | Var Name
  | App PTree PTree
  | Lam Name PTree
  deriving (Eq, Show)
