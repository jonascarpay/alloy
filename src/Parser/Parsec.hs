{-# LANGUAGE RankNTypes #-}

module Parser.Parsec
  ( Parser,
    runParser,
    token,
    offset,
    throw,
    throwCC,
  )
where

import Control.Applicative (Alternative (..))

{-# INLINE runParser #-}
runParser ::
  -- | Function to read a single token. Nothing means EOF/OOB.
  (Int -> t) ->
  Parser t e a ->
  Either (Int, e) a
runParser t (Parser p) = p t 0 (\a _ -> Right a) (\e i -> Left (i, e))

-- | A simple backtracking parser.
-- Maintains the error message of whatever branch managed to consume the most input.
newtype Parser t e a = Parser
  { unParser ::
      forall b.
      (Int -> t) -> -- Read token
      Int -> -- Current position
      (a -> Int -> b) -> -- Succes
      (e -> Int -> b) -> -- Error
      b
  }

instance Functor (Parser t e) where
  {-# INLINE fmap #-}
  fmap f (Parser k) = Parser $ \t i ok -> k t i (ok . f)

instance Applicative (Parser t e) where
  {-# INLINE pure #-}
  pure a = Parser $ \_ i ok _ -> ok a i
  {-# INLINE (<*>) #-}
  Parser pf <*> Parser pa = Parser $ \t i ok err -> pf t i (\f i' -> pa t i' (ok . f) err) err

instance Monad (Parser t e) where
  {-# INLINE (>>=) #-}
  Parser k >>= f = Parser $ \t i ok err -> k t i (\a i' -> unParser (f a) t i' ok err) err

instance Monoid e => Alternative (Parser t e) where
  {-# INLINE empty #-}
  empty = Parser $ \_ i _ err -> err mempty i
  {-# INLINE (<|>) #-}
  Parser pl <|> Parser pr = Parser $ \t i ok err ->
    pl t i ok $ \el sl ->
      pr t i ok $ \er sr ->
        case compare sl sr of
          LT -> err er sr
          EQ -> err (el <> er) sl
          GT -> err el sl

{-# INLINE token #-}
token :: Parser t e t
token = Parser $ \t i ok _ -> ok (t i) (i + 1)

{-# INLINE offset #-}
offset :: Parser t e Int
offset = Parser $ \_ i ok _ -> ok i i

{-# INLINE throw #-}
throw :: e -> Parser t e a
throw e = Parser $ \_ i _ err -> err e i

{-# INLINE throwCC #-}
throwCC :: ((forall err. e -> Parser t e err) -> Parser t e a) -> Parser t e a
throwCC k = Parser $ \t i ok err ->
  let throw' e = Parser $ \_ _ _ err' -> err' e i
   in unParser (k throw') t i ok err
