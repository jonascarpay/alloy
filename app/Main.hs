{-# LANGUAGE LambdaCase #-}

import Data.ByteString qualified as BS
import Data.ByteString.Char8 qualified as BS8
import Data.Foldable
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
  case parse input of
    Left err -> putStrLn err
    Right expr ->
      runEval expr >>= \case
        Left err -> die err
        Right nf -> BS8.putStrLn $ printNF nf

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
