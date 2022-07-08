module Language.Sleigh (
  -- * Parsing
    parseSleigh
  , PM.SleighParseError(..)
  , PP.SleighPreprocessingError(..)
  , SleighError(..)
  , module A
  -- * Preprocessing
  , PP.preprocessSleigh
  , PP.Token(..)
  , PP.Positioned(..)
  , LSI.Identifier(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE
import qualified Prettyprinter as PP
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh.AST as A
import qualified Language.Sleigh.Identifier as LSI
import qualified Language.Sleigh.ParserMonad as PM
import qualified Language.Sleigh.Parser as P
import qualified Language.Sleigh.Preprocessor as PP

data SleighError = PreprocessingError (TM.ParseErrorBundle DT.Text PP.SleighPreprocessingError)
                 | ParseError (TM.ParseErrorBundle PM.TokenStream PM.SleighParseError)

instance PP.Pretty SleighError where
  pretty e =
    case e of
      PreprocessingError eb -> PP.pretty (TM.errorBundlePretty eb)
      ParseError eb -> PP.pretty (TM.errorBundlePretty eb)

parseSleigh
  :: FilePath
  -- ^ The path to resolve includes relative to
  -> FilePath
  -- ^ The name of the sleigh file to use in error messages
  -> BS.ByteString
  -- ^ The contents of the sleigh file to parse (initial file only; includes will be resolved automatically)
  -> IO (Either SleighError A.Sleigh)
parseSleigh includePath filename contents = do
  etoks <- PP.preprocessSleigh includePath filename contents
  case etoks of
    Left ppErr -> return (Left (PreprocessingError ppErr))
    Right toks -> do
      let str0 = DTE.decodeUtf8With DTEE.lenientDecode contents
      eres <- PM.runSleigh filename str0 toks P.sleighParser
      case eres of
        Left e -> return (Left (ParseError e))
        Right s -> return (Right s)
