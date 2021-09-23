{-# LANGUAGE RankNTypes #-}

module Parser.Parsec
  ( Parser,
    runParser,
    token,
    throwAt,
  )
where

import Control.Applicative (Alternative (..))
import Control.Monad (MonadPlus)

{-# INLINE runParser #-}
runParser ::
  Monoid e =>
  (Int -> t) ->
  Parser t e a ->
  Either (Int, e) a
runParser t (Parser p) = p t 0 mempty (\a _ _ -> Right a) (\(Err i e) -> Left (i, e))

data Err e = Err Int e

instance Semigroup e => Semigroup (Err e) where
  Err il el <> Err ir er = case compare il ir of
    LT -> Err ir er
    EQ -> Err ir (el <> er)
    GT -> Err il el

instance Monoid e => Monoid (Err e) where
  mempty = Err 0 mempty

-- | A simple backtracking parser.
-- Maintains the error message of whatever branch managed to consume the most input.
-- Reducing the state to an integer means we can associate errors with the index at which they occurred.
newtype Parser t e a = Parser
  { unParser ::
      forall b.
      (Int -> t) -> -- Read token
      Int -> -- Offset
      Err e -> -- error set
      (a -> Int -> Err e -> b) -> -- Success continuation
      (Err e -> b) -> -- Error continuation
      b
  }

instance Functor (Parser t e) where
  {-# INLINE fmap #-}
  fmap f (Parser k) = Parser $ \t i e ok -> k t i e (ok . f)

instance Applicative (Parser t e) where
  {-# INLINE pure #-}
  pure a = Parser $ \_ i e ok _ -> ok a i e
  {-# INLINE (<*>) #-}
  Parser pf <*> Parser pa = Parser $ \t i e ok ng -> pf t i e (\f i' e' -> pa t i' e' (ok . f) ng) ng

instance Monad (Parser t e) where
  {-# INLINE (>>=) #-}
  Parser k >>= f = Parser $ \t i e ok ng -> k t i e (\a i' e' -> unParser (f a) t i' e' ok ng) ng

instance Monoid e => Alternative (Parser t e) where
  {-# INLINE empty #-}
  empty = Parser $ \_ _ e _ ng -> ng e
  {-# INLINE (<|>) #-}
  Parser pl <|> Parser pr = Parser $ \t i e ok ng ->
    pl t i e ok $ \e' ->
      pr t i e' ok ng

instance Monoid e => MonadPlus (Parser t e)

{-# INLINE token #-}
token :: Parser t e t
token = Parser $ \t i e ok _ -> ok (t i) (i + 1) e

-- | Enter a context with a function to throw an error at the start of the context.
-- A simple motivating example is `expect`:
--
-- > expect :: Error -> (Token -> Maybe a) -> Parser a
-- > expect err f =
-- >   P.throwAt $ \throw ->
-- >      P.token >>= maybe (throw err) pure . f
--
-- In this example, we first consume a token, and then see if it matches our expectation.
-- However since consuming a token advanced the parser past the token, simply throwing an error in place reports the error after the token.
--
-- By having 'throwAt' be the only way to throw errors, it's always clear and explicit where you are reporting an error.
{-# INLINE throwAt #-}
throwAt :: Semigroup e => ((forall a. e -> Parser t e a) -> Parser t e r) -> Parser t e r
throwAt k = Parser $ \t i e ok err ->
  let throw' e = Parser $ \_ _ e' _ err' -> err' (e' <> Err i e)
   in unParser (k throw') t i e ok err
