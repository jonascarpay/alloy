{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Print.Printer
  ( Printer,
    runPrinter,
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
import Data.String
import Lens.Micro.Platform

newtype PrinterContext = PrinterContext {_pcIndent :: Int}

data PrinterState = PrinterState
  { _psBuilder :: Builder,
    _psColumn :: Int
  }

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
    s0 = PrinterState mempty 0

indent :: Printer -> Printer
indent (Printer m) =
  Printer $
    local (over pcIndent (+ 2)) $ spacer >> m
  where
    spacer :: RawPrinter ()
    spacer = do
      col <- use psColumn
      ind <- view pcIndent
      case compare col ind of
        LT -> emitRaw (const ind) (replicate (ind - col) ' ')
        EQ -> pure ()
        GT -> unPrinterT newline

newline :: Printer
newline = Printer $ do
  i <- view pcIndent
  emitRaw (const i) ("\n" <> replicate i ' ')

{-# INLINE emitRaw #-}
emitRaw :: (Int -> Int) -> String -> RawPrinter ()
emitRaw fcol s = do
  psBuilder %= flip mappend (BSB.string7 s)
  psColumn %= fcol

emit :: String -> Printer
emit s = Printer $ emitRaw (+ length s) s

emits :: Show a => a -> Printer
emits = emit . show
