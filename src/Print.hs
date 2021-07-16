{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}

module Print where

import Control.Monad.Identity
import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Eval.Types

newtype PrinterT m a = PrinterT {unPrinterT :: ReaderT Int (StateT Builder m) a}
  deriving (Functor, Monad, Applicative, MonadIO)

instance MonadTrans PrinterT where
  lift = PrinterT . lift . lift

runPrinterT :: Monad m => PrinterT m () -> m ByteString
runPrinterT (PrinterT m) = BSL.toStrict . BSB.toLazyByteString <$> execStateT (runReaderT m 0) mempty

indent :: Monad m => PrinterT m a -> PrinterT m a
indent (PrinterT m) = PrinterT $ local (+ 2) m

{-# INLINE spit #-}
spit :: Monad m => Builder -> PrinterT m ()
spit s = PrinterT $ state $ \b -> ((), b <> s)

{-# INLINE newline #-}
newline :: Monad m => PrinterT m ()
newline = do
  i <- PrinterT ask
  spit "\n"
  replicateM_ i $ spit " "

newtype NameT m a = NameT {unNameT :: StateT Int m a}
  deriving (Functor, Applicative, Monad)

runNameT :: Monad m => NameT m a -> m a
runNameT (NameT m) = evalStateT m 0

printNF :: NF -> ByteString
printNF = runIdentity . runNameT . runPrinterT . pNF

pNF :: Monad m => NF -> PrinterT (NameT m) ()
pNF = go . unNF
  where
    go (VClosure _ _) = spit "<closure>"
    go (VFunc mod hash) = spit "<function>"
    go (VType typ) = spit "<type>"
    go (VVar ix) = spit "<var>"
    go (VBlk blk) = spit "<block ref>"
    go (VAttr attr) = spit "<attr set>"
    go (VRun deps run) = spit "<runtime expression>"
