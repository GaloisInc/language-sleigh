module Main ( main ) where

import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Options.Applicative as OA
import qualified Prettyprinter as PP
import qualified Prettyprinter.Render.Text as PRT
import qualified System.IO as IO
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh as LS

data Mode = Preprocess | Parse
  deriving (Show)

data Options =
  Options { includePath :: FilePath
          , targetFile :: FilePath
          , mode :: Mode
          }

optionParser :: OA.Parser Options
optionParser =
  Options <$> OA.strOption ( OA.long "include-path"
                           <> OA.short 'I'
                           <> OA.metavar "DIR"
                           <> OA.help "The directory to resolve includes relative to"
                           )
          <*> OA.strArgument ( OA.metavar "FILE"
                             <> OA.help "The Sleigh file to parse"
                             )
          <*> OA.flag Parse Preprocess ( OA.short 'E'
                                       <> OA.long "preprocess"
                                       <> OA.showDefault
                                       <> OA.help "Mode to execute in"
                                       )

mainWith :: Options -> IO ()
mainWith opts = do
  targetBytes <- BS.readFile (targetFile opts)
  case mode opts of
    Parse -> do
      res <- LS.parseSleigh (includePath opts) (targetFile opts) targetBytes
      case res of
        Right s -> PRT.hPutDoc IO.stdout (PP.pretty s) >> IO.hPutStrLn IO.stdout ""
        Left e -> IO.hPutStr IO.stdout (TM.errorBundlePretty e)
    Preprocess -> do
      res <- LS.preprocessSleigh (includePath opts) (targetFile opts) targetBytes
      case res of
        Left err -> IO.hPutStr IO.stdout (TM.errorBundlePretty err)
        Right tokens -> PRT.hPutDoc IO.stdout (PP.vcat (fmap PP.pretty (F.toList tokens)))

main :: IO ()
main = mainWith =<< OA.execParser opts
  where
    opts = OA.info (optionParser OA.<**> OA.helper)
                   (  OA.fullDesc
                   <> OA.progDesc "Run the Sleigh file parser"
                   )
