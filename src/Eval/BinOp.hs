module Eval.BinOp where

import Control.Monad.Except
import Data.Sequence (Seq)
import Data.Text (Text)
import Eval.Types
import Expr

{-# INLINE compOp #-}
compOp :: Ord a => CompOp -> a -> a -> Bool
compOp Eq = (==)
compOp Neq = (/=)
compOp Lt = (<)
compOp Gt = (>)
compOp Leq = (<=)
compOp Geq = (>=)

rtBinOp ::
  BinOp ->
  RTValue var blk fun lit () ->
  RTValue var blk fun lit () ->
  Comp (RTValue var blk fun lit ())
rtBinOp (ArithOp op) l r = pure $ RTArith op l r ()
rtBinOp (CompOp op) l r = pure $ RTComp op l r ()

-- TODO
-- Ideally, there is a way make this a single function that describes both
-- compile time and run time behaviour.  At compile time, it'd describe the
-- evaluation and at run time, it'd describe type checking?
-- Currently, we don't reject invalid arithmetic expressions at all.
-- Might not make sense because of e.g. pointer arithmetic?
binPrim :: BinOp -> Prim -> Prim -> Eval WHNF
binPrim op (PInt a) (PInt b) = VPrim <$> binInt op a b
binPrim op (PDouble a) (PDouble b) = VPrim <$> binDouble op a b
binPrim op (PDouble a) (PInt b) = VPrim <$> binDouble op a (fromIntegral b)
binPrim op (PInt a) (PDouble b) = VPrim <$> binDouble op (fromIntegral a) b

binInt :: BinOp -> Int -> Int -> Eval Prim
binInt (ArithOp op) l r = pure . PInt $ arithInt op l r
  where
    arithInt :: ArithOp -> Int -> Int -> Int
    arithInt Add = (+)
    arithInt Sub = (-)
    arithInt Mul = (*)
    arithInt Div = div
binInt (CompOp op) l r = pure . PBool $ compOp op l r

binDouble :: BinOp -> Double -> Double -> Eval Prim
binDouble (ArithOp op) l r = pure . PDouble $ arithDouble op l r
  where
    arithDouble :: ArithOp -> Double -> Double -> Double
    arithDouble Add = (+)
    arithDouble Sub = (-)
    arithDouble Mul = (*)
    arithDouble Div = (/)
binDouble (CompOp op) l r = pure . PBool $ compOp op l r

binString :: BinOp -> Text -> Text -> Eval WHNF
binString (CompOp op) l r = pure . VPrim . PBool $ compOp op l r
binString (ArithOp Add) l r = pure . VString $ l <> r
binString (ArithOp _) _ _ = throwError "Cannot perform arithmetic on strings"

binList :: BinOp -> Seq Thunk -> Seq Thunk -> Eval WHNF
binList (ArithOp Add) l r = pure . VList $ l <> r
binList (ArithOp _) _ _ = throwError "Cannot perform arithmetic on strings"
binList (CompOp _) _ _ = throwError "Cannot compare lists"

-- binList (CompOp op) l r = pure . VPrim . PBool . fromOrdering op <$> compareLists
--   where
--     compareLists :: Seq Thunk -> Seq Thunk ->

-- TODO Comparisons for recursive values
-- Probably convert to a ComparableValue, and then compare those?
-- {-# INLINE fromOrdering #-}
-- fromOrdering :: CompOp -> Ordering -> Bool
-- fromOrdering Eq EQ = True
-- fromOrdering Eq _ = False
-- fromOrdering Neq EQ = False
-- fromOrdering Neq _ = True
-- fromOrdering Lt LT = True
-- fromOrdering Lt _ = False
-- fromOrdering Gt GT = True
-- fromOrdering Gt _ = False
-- fromOrdering Leq GT = False
-- fromOrdering Leq _ = True
-- fromOrdering Geq LT = False
-- fromOrdering Geq _ = True
