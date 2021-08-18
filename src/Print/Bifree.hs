module Print.Bifree where

import Data.Bifunctor

newtype Bifix flip self = Bifix (self (Bifix self flip) (Bifix flip self))

bicata :: (Bifunctor g, Bifunctor f) => (g a a -> a) -> (f a a -> a) -> Bifix g f -> a
bicata fg ff (Bifix f) = ff $ bimap (bicata ff fg) (bicata fg ff) f
