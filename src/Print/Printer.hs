{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Printer
  ( Printer,
    runPrinter,
    indent,
    space,
    newline,
    align,
    emit,
    emits,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString
import Data.ByteString.Builder (Builder)
import Data.ByteString.Builder qualified as BSB
import Data.ByteString.Lazy qualified as BSL
import Data.Semigroup
import Data.String
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

newtype Printer = Printer {unPrinterT :: ReaderT PrinterContext (State PrinterState) ()}

instance Semigroup Printer where
  Printer a <> Printer b = Printer (a >> b)

instance Monoid Printer where
  mempty = Printer (pure ())

instance IsString Printer where
  fromString = emit . fromString

runPrinter :: Printer -> ByteString
runPrinter (Printer m) = BSL.toStrict . BSB.toLazyByteString . view psBuilder $ execState (runReaderT m c0) s0
  where
    c0 = PrinterContext 0
    s0 = PrinterState mempty 0 RequestedNone

-- TODO could be implemented just using align
indent :: Printer -> Printer
indent (Printer m) = Printer $ local (over pcIndent (+ 2)) m

align :: Printer -> Printer
align (Printer m) = Printer $ do
  unPrinterT emitSpacer
  col <- use psColumn
  local (pcIndent .~ col) m

space :: Printer
space = Printer $ psDelimiter %= max RequestedSpace

newline :: Printer
newline = Printer $ psDelimiter %= max RequestedNewline

{-# INLINE emitSpacer #-}
emitSpacer :: Printer
emitSpacer =
  Printer $ do
    use psDelimiter >>= \case
      RequestedNone -> pure ()
      RequestedSpace -> unPrinterT $ emitRaw " "
      RequestedNewline -> view pcIndent >>= \i -> unPrinterT (emitRaw $ "\n" <> stimes i " ")
    psDelimiter .= RequestedNone

{-# INLINE emitRaw #-}
emitRaw :: Builder -> Printer
emitRaw s = Printer $ psBuilder %= flip mappend s

emit :: Builder -> Printer
emit s = emitSpacer <> emitRaw s

emits :: Show a => a -> Printer
emits = emit . BSB.stringUtf8 . show
