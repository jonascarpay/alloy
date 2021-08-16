{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Print (printNF) where

import Control.Monad.Identity
import Control.Monad.State
import Data.ByteString
import Eval.Types
import Expr
import Print.Printer

type Prints a = a -> PrinterT (NameT Identity) ()

newtype NameT m a = NameT {unNameT :: StateT Int m a}
  deriving (Functor, Applicative, Monad)

runNameT :: Monad m => NameT m a -> m a
runNameT (NameT m) = evalStateT m 0

printNF :: NF -> ByteString
printNF = runIdentity . runNameT . runPrinterT . pNF

pPrim :: Prints Prim
pPrim (PInt n) = spits n
pPrim (PDouble n) = spits n
pPrim (PBool n) = spits n
pPrim PVoid = spit "<void>"

pArithOp :: Prints ArithOp
pArithOp Add = spit "+"
pArithOp Div = spit "/"
pArithOp Mul = spit "*"
pArithOp Sub = spit "-"

pCompOp :: Prints CompOp
pCompOp Eq = spit "=="

parens :: Monad m => PrinterT m a -> PrinterT m a
parens m = spit "(" *> m <* spit ")"

binop = operator opPrec pOp pValue "(" ")" True

pValue :: Prints (RTValue typ var lbl fun)
pValue (RTArith op a b _) = operator opPrec pValue op a b
pValue (RTPrim p _) = pPrim p

pNF :: Prints NF
pNF = go . unNF
  where
    go VClosure {} = spit "<closure>"
    go VRTPlace {} = spit "<runtime place>"
    go (VRTValue deps val) = pValue val
    go VFunc {} = spit "<function>"
    go VType {} = spit "<type>"
    go VBlk {} = spit "<block ref>"
    go VAttr {} = spit "<attr set>"
    go (VPrim p) = pPrim p
    go VString {} = spit "<string>"
    go VList {} = spit "<list>"
