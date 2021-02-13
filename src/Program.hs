module Program where

import Data.Map (Map)
import Expr

data Closure a = Closure
  { functions :: Map Name Function,
    closed :: a
  }

data Module

data Function

newtype Block = Block {unBlock :: [Statement]}

data Statement
