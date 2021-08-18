{-# LANGUAGE DeriveTraversable #-}

module Print.Render where

data Cofree2 flip self a = a :< self (Cofree2 self flip) (Cofree2 flip self)
  deriving (Functor, Foldable, Traversable)

-- render :: Doc ->
