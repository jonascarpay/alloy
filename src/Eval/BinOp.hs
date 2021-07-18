module Eval.BinOp where

import Control.Monad.Except
import Data.ByteString (ByteString)
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

rtBinOp :: BinOp -> RTVal VarIX BlockIX Hash -> RTVal VarIX BlockIX Hash -> Comp (RTVal VarIX BlockIX Hash)
rtBinOp (ArithOp op) l r = pure $ RTArith op l r
rtBinOp (CompOp op) l r = pure $ RTComp op l r

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

binString :: BinOp -> ByteString -> ByteString -> Eval WHNF
binString (CompOp op) l r = pure . VPrim . PBool $ compOp op l r
binString (ArithOp Add) l r = pure . VString $ l <> r
binString (ArithOp _) _ _ = throwError "Cannot perform arithmetic on strings"
