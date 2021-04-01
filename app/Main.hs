import Data.Foldable
import Lib
import Options.Applicative
import Parse
import Print
import Text.Megaparsec qualified as MP

newtype Command = Evaluate FilePath

commandParser :: Parser Command
commandParser = Evaluate <$> strOption (long "input" <> short 'f' <> metavar "FILENAME" <> help "input file")

runCommand :: Command -> IO ()
runCommand (Evaluate fp) = do
  input <- readFile fp
  case MP.parse pToplevel fp input of
    Left err -> putStrLn $ MP.errorBundlePretty err
    Right expr -> do
      evalInfo fp expr >>= either print (print . ppVal)

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
