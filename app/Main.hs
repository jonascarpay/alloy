import Data.ByteString qualified as BS
import Data.Foldable
import Lib
import Options.Applicative
import Parser.Parser
import Print

newtype Command = Evaluate FilePath

commandParser :: Parser Command
commandParser = Evaluate <$> strOption (long "input" <> short 'f' <> metavar "FILENAME" <> help "input file")

runCommand :: Command -> IO ()
runCommand (Evaluate fp) = do
  input <- BS.readFile fp
  case parse input of
    Left err -> putStrLn err
    Right expr -> evalInfo fp expr >>= either print (print . ppVal)

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
