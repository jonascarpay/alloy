{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}

module Print (printNF) where

import Control.Monad.Identity
import Control.Monad.State
import Data.Bool
import Data.ByteString (ByteString)
import Data.ByteString.Builder (Builder)
import Data.Map (Map)
import Data.Sequence (Seq)
import Data.Sequence qualified as Seq
import Eval.Lib
import Eval.Types
import Expr
import Print.Printer

type Prints a = a -> Printer

printNF :: NF -> ByteString
printNF = runIdentity . runNameT . runPrinterT . pNF

pPrim :: Prints Prim
pPrim (PInt n) = emits n
pPrim (PDouble n) = emits n
pPrim (PBool n) = emits n
pPrim PVoid = emit "<void>"

pProg :: Prints (RTProg var lbl fun Type)
pProg = undefined

pNF :: Prints NF
pNF = go . unNF
  where
    go VClosure {} = emit "<closure>"
    go VRTPlace {} = emit "<runtime place>"
    go (VRTValue deps val) = pValue 0 $ valStyle val
    go VFunc {} = emit "<function>"
    go VType {} = emit "<type>"
    go VBlk {} = emit "<block ref>"
    go VAttr {} = emit "<attr set>"
    go (VPrim p) = pPrim p
    go VString {} = emit "<string>"
    go VList {} = emit "<list>"
