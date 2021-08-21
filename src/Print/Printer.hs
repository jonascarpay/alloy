{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Printer
  ( Printer,
    runPrinter,
    space,
    newline,
    indent,
    emit,
    emits,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.ByteString (ByteString)
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
    _psDelimiter :: RequestedSpacer,
    _psFreshIndent :: Bool
  }

data RequestedSpacer
  = RequestedNone
  | RequestedSpace
  | RequestedNewline
  deriving (Eq, Ord)

makeLenses ''PrinterContext
makeLenses ''PrinterState

type RawPrinter = ReaderT PrinterContext (State PrinterState)

newtype Printer = Printer {unPrinterT :: RawPrinter ()}

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
    s0 = PrinterState mempty 0 RequestedNone True

indent :: Printer -> Printer
indent (Printer m) = Printer $ do
  use psFreshIndent >>= \case
    True -> m
    False -> local (over pcIndent (+ 2)) $ do
      psFreshIndent .= True
      spacer
      m
  where
    spacer :: RawPrinter ()
    spacer = do
      col <- use psColumn
      ind <- view pcIndent
      case compare col ind of
        LT -> emitRaw (const ind) (replicate (ind - col) ' ')
        EQ -> pure ()
        GT -> unPrinterT newline

space :: Printer
space = Printer $ psDelimiter %= max RequestedSpace

newline :: Printer
newline = Printer $ psDelimiter %= max RequestedNewline

{-# INLINE emitSpacer #-}
emitSpacer :: RawPrinter ()
emitSpacer = do
  use psDelimiter >>= \case
    RequestedNone -> pure ()
    RequestedSpace -> emitRaw (+ 1) " "
    RequestedNewline -> do
      i <- view pcIndent
      emitRaw id "\n"
      emitRaw (const i) $ mtimesDefault i " "
  psDelimiter .= RequestedNone

{-# INLINE emitRaw #-}
emitRaw :: (Int -> Int) -> String -> RawPrinter ()
emitRaw fcol s = do
  psBuilder %= flip mappend (BSB.string7 s)
  psColumn %= fcol

emit :: String -> Printer
emit s = Printer $ do
  psFreshIndent .= False
  emitSpacer
  emitRaw (+ length s) s

emits :: Show a => a -> Printer
emits = emit . show
