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
pPrim (PInt n) = emits n
pPrim (PDouble n) = emits n
pPrim (PBool n) = emits n
pPrim PVoid = emit "<void>"

pArithOp :: Prints ArithOp
pArithOp Add = emit "+"
pArithOp Div = emit "/"
pArithOp Mul = emit "*"
pArithOp Sub = emit "-"

pCompOp :: Prints CompOp
pCompOp Eq = emit "=="

parens :: Monad m => PrinterT m a -> PrinterT m a
parens m = space *> emit "(" *> m <* emit ")"

pValue :: Prints (RTValue typ var lbl fun)
pValue (RTPrim p _) = pPrim p

pNF :: Prints NF
pNF = go . unNF
  where
    go VClosure {} = emit "<closure>"
    go VRTPlace {} = emit "<runtime place>"
    go (VRTValue deps val) = pValue val
    go VFunc {} = emit "<function>"
    go VType {} = emit "<type>"
    go VBlk {} = emit "<block ref>"
    go VAttr {} = emit "<attr set>"
    go (VPrim p) = pPrim p
    go VString {} = emit "<string>"
    go VList {} = emit "<list>"
