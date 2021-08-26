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
    emitText,
    emitShow,
  )
where

import Control.Monad.Reader
import Control.Monad.State
import Data.Semigroup (mtimesDefault, stimes)
import Data.String
import Data.Text (Text)
import Data.Text qualified as T
import Data.Text.Lazy qualified as TL
import Data.Text.Lazy.Builder (Builder)
import Data.Text.Lazy.Builder qualified as TB
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

runPrinter :: Printer -> Text
runPrinter (Printer m) = TL.toStrict . TB.toLazyText . view psBuilder $ execState (runReaderT m c0) s0
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
        LT -> emitRaw (const ind) (stimes (ind - col) " ")
        EQ -> pure ()
        GT -> unPrinterT newline

newline :: Printer
newline = Printer $ do
  i <- view pcIndent
  emitRaw (const i) ("\n" <> mtimesDefault i " ")

{-# INLINE emitRaw #-}
emitRaw :: (Int -> Int) -> Builder -> RawPrinter ()
emitRaw fcol s = do
  psBuilder %= flip mappend s
  psColumn %= fcol

emit :: String -> Printer
emit s = Printer $ emitRaw (+ length s) (TB.fromString s)

emitText :: Text -> Printer
emitText t = Printer $ emitRaw (+ T.length t) (TB.fromText t)

emitShow :: Show a => a -> Printer
emitShow = emit . show
