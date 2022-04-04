module Language.Sleigh (
  -- * Parsing
    parseSleigh
  , PM.SleighParseError(..)
  , A.Sleigh(..)
  -- * Preprocessing
  , PP.preprocessSleigh
  , PP.Token(..)
  , PP.Positioned(..)
  ) where

import qualified Data.ByteString as BS
import qualified Data.Text as DT
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh.AST as A
import qualified Language.Sleigh.ParserMonad as PM
import qualified Language.Sleigh.Parser as P
import qualified Language.Sleigh.Preprocessor as PP

parseSleigh
  :: FilePath
  -- ^ The path to resolve includes relative to
  -> FilePath
  -- ^ The name of the sleigh file to use in error messages
  -> BS.ByteString
  -- ^ The contents of the sleigh file to parse (initial file only; includes will be resolved automatically)
  -> IO (Either (TM.ParseErrorBundle DT.Text PM.SleighParseError) A.Sleigh)
parseSleigh includePath filename contents =
  PM.runSleigh includePath filename contents P.sleighParser
