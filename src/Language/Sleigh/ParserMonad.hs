{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
module Language.Sleigh.ParserMonad (
    SleighParseError(..)
  , SleighM
  , runSleigh
  -- * State modifiers
  , recordDefinition
  , recordPreprocessorDefinition
  , undefinePreprocessor
  , processInclude
  -- * State inspection
  , definitions
  ) where

import           Control.Applicative ( Alternative )
import qualified Control.Lens as CL
import qualified Control.Lens.TH as CLT
import           Control.Monad ( MonadPlus )
import           Control.Monad.IO.Class ( liftIO )
import qualified Control.Monad.RWS.Strict as CMR
import qualified Data.ByteString as BS
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE
import qualified Prettyprinter as PP
import           System.FilePath ( (</>) )
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh.AST as A

-- | State required to parse sleigh files
--
-- This includes preprocessor state and any extra information required from includes
data ParserState =
  ParserState { _parserDefines :: Map.Map A.Identifier DT.Text
              -- ^ Current preprocessor definitions
              --
              -- Note that these are added and removed during parsing
              , _definedStmts :: Seq.Seq A.Definition
              -- ^ Language-level define statements
              }

$(CLT.makeLenses ''ParserState)

data IncludeContext =
  IncludeContext { fileIssuingInclude :: FilePath
                   -- ^ The file containing the include statement
                 , includePos :: TM.SourcePos
                   -- ^ The line number in the including file that the include occurs on
                 }
  deriving (Eq, Ord, Show)

-- | Constant environment data for the parser
data ParserEnv =
  ParserEnv { parserWorkingDirectory :: FilePath
            -- ^ The directory that includes should be resolved with respect to
            --
            -- This is specified separately from the process working directory
            -- to enable us to run the parser from anywhere relative to the
            -- location of the source files
            , contextStack :: [IncludeContext]
            -- ^ The stack of includes
            , thisFile :: FilePath
            -- ^ The name of this file
            }

newtype WrappedErrorBundle = WrappedErrorBundle (TM.ParseErrorBundle DT.Text SleighParseError)
  deriving (Eq, Show)

instance Ord WrappedErrorBundle where
  compare (WrappedErrorBundle b1) (WrappedErrorBundle b2) =
    compare (TM.errorBundlePretty b1) (TM.errorBundlePretty b2)

-- | Parser errors from sleigh semantics (rather than pure syntax errors, which are reported by megaparsec)
data SleighParseError = ErrorInIncludedFile [IncludeContext] WrappedErrorBundle
  deriving (Eq, Ord, Show)


instance PP.Pretty IncludeContext where
  pretty ic = PP.pretty (fileIssuingInclude ic) <> PP.pretty ":" <> PP.pretty (TM.sourcePosPretty (includePos ic))

instance TM.ShowErrorComponent SleighParseError where
  showErrorComponent e =
    case e of
      ErrorInIncludedFile ctxs (WrappedErrorBundle err) ->
        show $ PP.vcat [ PP.pretty "Error in included file:"
                       , PP.indent 2 (PP.vcat [PP.pretty ic | ic <- ctxs])
                       , PP.pretty (TM.errorBundlePretty err)
                       ]

-- | A monad for parsing Sleigh that includes the necessary state
--
-- Note that while this is a transformer over IO, we don't export the full power
-- of IO. We only expose the primitives necessary to handle include files and
-- other restricted operations.
newtype SleighM a = SleighM { unSleigh :: TM.ParsecT SleighParseError DT.Text (CMR.RWST ParserEnv () ParserState IO) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , TM.MonadParsec SleighParseError DT.Text
           )

recordPreprocessorDefinition
  :: A.Identifier
  -> DT.Text
  -> SleighM ()
recordPreprocessorDefinition ident val = SleighM $ parserDefines CL.%= Map.insert ident val

-- | Should it be an error to undefine something that wasn't defined? Lets say no
undefinePreprocessor
  :: A.Identifier
  -> SleighM ()
undefinePreprocessor ident = SleighM $ parserDefines CL.%= Map.delete ident

recordDefinition
  :: A.Definition
  -> SleighM ()
recordDefinition d = SleighM $ definedStmts CL.%= (Seq.|> d)

definitions :: SleighM (Seq.Seq A.Definition)
definitions = SleighM (CL.use definedStmts)

-- | Process a preprocessor include
--
-- This reads the file and recursively invokes the parser (capturing the state
-- and saving any updates made by the recursive invocation).
--
-- Note that this does not update the ParsecT state, as the sub-parse is totally
-- separate.  If the sub-parse fails, we:

-- 1. Explicitly traverse the returned errors and add them to the current parser state
-- 2. Trigger a parse failure
processInclude
  :: DT.Text
  -> SleighM ()
  -> SleighM ()
processInclude filename parseWith = SleighM $ do
  incDir <- CMR.asks parserWorkingDirectory
  let fullPath = incDir </> DT.unpack filename
  bytes <- liftIO $ BS.readFile fullPath
  let txt = DTE.decodeUtf8With DTEE.lenientDecode bytes
  env <- CMR.ask
  currentState <- CMR.get

  thisPos <- TM.getSourcePos
  let thisContext = IncludeContext { fileIssuingInclude = thisFile env
                                   , includePos = thisPos
                                   }
  let env' = env { contextStack = thisContext : contextStack env
                 , thisFile = fullPath
                 }

  (res, nextState, _) <- liftIO $ CMR.runRWST (TM.runParserT (unSleigh parseWith) (DT.unpack filename) txt) env' currentState
  CMR.put nextState
  case res of
    Right () -> return ()
    Left errs -> do
      ctxts <- CMR.asks contextStack
      TM.customFailure (ErrorInIncludedFile ctxts (WrappedErrorBundle errs))

-- | Run a 'SleighM' parser
--
-- Note that this must be in 'IO' because it resolves includes dynamically
runSleigh
  :: FilePath
  -- ^ The path to resolve includes relative to
  -> FilePath
  -- ^ The name of the sleigh file to use in error messages
  -> BS.ByteString
  -- ^ The contents of the sleigh file to parse (initial file only; includes will be resolved automatically)
  -> SleighM a
  -- ^ The parser to run
  -> IO (Either (TM.ParseErrorBundle DT.Text SleighParseError) a)
runSleigh includePath filename contents p =
  fst <$> CMR.evalRWST (TM.runParserT (unSleigh p) filename txt) env s0
  where
    env = ParserEnv { parserWorkingDirectory = includePath
                    , contextStack = []
                    , thisFile = filename
                    }
    s0 = ParserState { _parserDefines = Map.empty
                     , _definedStmts = mempty
                     }
    txt = DTE.decodeUtf8With DTEE.lenientDecode contents
