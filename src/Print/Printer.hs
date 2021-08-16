{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Printer (PrinterT, runPrinterT, indent, space, newline, align, emit, emits) where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Semigroup
import Lens.Micro.Platform

newtype PrinterContext = PrinterContext
  { _pcIndent :: Int
  }

data PrinterState = PrinterState
  { _psBuilder :: Builder,
    _psColumn :: Int,
    _psDelimiter :: RequestedSpacer
  }

data RequestedSpacer
  = RequestedNone
  | RequestedSpace
  | RequestedNewline
  deriving (Eq, Ord)

makeLenses ''PrinterContext
makeLenses ''PrinterState

newtype PrinterT m a = PrinterT {unPrinterT :: ReaderT PrinterContext (StateT PrinterState m) a}
  deriving (Functor, Monad, Applicative, MonadIO)

instance MonadTrans PrinterT where
  lift = PrinterT . lift . lift

runPrinterT :: Monad m => PrinterT m () -> m ByteString
runPrinterT (PrinterT m) = BSL.toStrict . BSB.toLazyByteString . view psBuilder <$> execStateT (runReaderT m c0) s0
  where
    c0 = PrinterContext 0
    s0 = PrinterState mempty 0 RequestedNone

indent :: Monad m => PrinterT m a -> PrinterT m a
indent (PrinterT m) = PrinterT $ local (over pcIndent (+ 2)) m

align :: Monad m => PrinterT m a -> PrinterT m a
align (PrinterT m) = PrinterT $ do
  unPrinterT emitSpacer
  col <- use psColumn
  local (pcIndent .~ col) m

space :: Monad m => PrinterT m ()
space = PrinterT $ psDelimiter %= max RequestedSpace

newline :: Monad m => PrinterT m ()
newline = PrinterT $ psDelimiter %= max RequestedNewline

{-# INLINE emitSpacer #-}
emitSpacer :: Monad m => PrinterT m ()
emitSpacer =
  PrinterT $ do
    use psDelimiter >>= \case
      RequestedNone -> pure ()
      RequestedSpace -> unPrinterT $ emitRaw " "
      RequestedNewline -> view pcIndent >>= \i -> unPrinterT (emitRaw $ "\n" <> stimes i " ")
    psDelimiter .= RequestedNone

{-# INLINE emitRaw #-}
emitRaw :: Monad m => Builder -> PrinterT m ()
emitRaw s = PrinterT $ psBuilder %= flip mappend s

emit :: Monad m => Builder -> PrinterT m ()
emit s = emitSpacer >> emitRaw s

emits :: (Show a, Monad m) => a -> PrinterT m ()
emits = emit . BSB.stringUtf8 . show
