module Lib.Fix where

newtype Fix f = Fix (f (Fix f))
