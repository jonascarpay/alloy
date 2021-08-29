{-# LANGUAGE LambdaCase #-}

import Data.ByteString qualified as BS
import Data.Foldable
import Data.Text.IO qualified as T
import Eval
import Options.Applicative
import Parser.Parser
import Print
import System.Exit

newtype Command = Evaluate FilePath

commandParser :: Parser Command
commandParser =
  Evaluate
    <$> strOption (long "input" <> short 'f' <> metavar "FILENAME" <> help "input file")

runCommand :: Command -> IO ()
runCommand (Evaluate fp) = do
  input <- BS.readFile fp
  path <- mkAbsPath fp
  case parse input path of
    Left err -> putStrLn err
    Right expr ->
      runEval expr >>= \case
        Left err -> die err
        Right nf -> T.putStrLn $ printNF nf

main :: IO ()
main = execParser opts >>= runCommand
  where
    opts = info (helper <*> commandParser) infoMod
    infoMod =
      fold
        [ fullDesc,
          progDesc "the compiler",
          header "the compiler -- official header"
        ]
