{-# LANGUAGE LambdaCase #-}

import Data.Foldable
import Data.Text.IO qualified as T
import Eval
import Options.Applicative
import Print
import System.Exit

newtype Command = Evaluate FilePath

commandParser :: Parser Command
commandParser =
  Evaluate
    <$> strOption (long "input" <> short 'f' <> metavar "FILENAME" <> help "input file")

runCommand :: Command -> IO ()
runCommand (Evaluate fp) = do
  runEvalFile fp >>= \case
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
