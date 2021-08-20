{-# LANGUAGE DeriveTraversable #-}

module Print.Bifree where

import Data.Bifunctor

-- | Mutually recursive fixpoint over two bifunctors
newtype Bifix verso recto = Bifix (recto (Bifix recto verso) (Bifix verso recto))

data Bifree verso recto a = Bipure a | Bifree (recto (Bifree recto verso a) (Bifree verso recto a))

instance (Bifunctor g, Bifunctor f) => Functor (Bifree g f) where
  fmap fn (Bipure a) = Bipure (fn a)
  fmap fn (Bifree f) = Bifree (bimap (fmap fn) (fmap fn) f)

data Bicofree verso recto a = (:<)
  { biextract :: a,
    biunwrap :: recto (Bicofree recto verso a) (Bicofree verso recto a)
  }

instance (Bifunctor g, Bifunctor f) => Functor (Bicofree g f) where
  fmap fn (a :< f) = fn a :< bimap (fmap fn) (fmap fn) f

bicata :: (Bifunctor g, Bifunctor f) => (g a a -> a) -> (f a a -> a) -> Bifix g f -> a
bicata fg ff (Bifix f) = ff $ bimap (bicata ff fg) (bicata fg ff) f

toBicofree :: (Bifunctor g, Bifunctor f) => (g a a -> a) -> (f a a -> a) -> Bifix g f -> Bicofree g f a
toBicofree fg ff (Bifix f) =
  let f' = bimap (toBicofree ff fg) (toBicofree fg ff) f
   in ff (bimap biextract biextract f') :< f'
