{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
-- | This is a preprocessor for Sleigh that resolves all of the preprocessor
-- definitions and directives, while (mostly) preserving source location
-- information
module Language.Sleigh.Preprocessor (
    preprocessSleigh
  , Token(..)
  , Positioned(..)
  ) where

import           Control.Applicative ( Alternative, (<|>), empty )
import           Control.Lens ( (%=), (.=) )
import qualified Control.Lens as CL
import qualified Control.Lens.TH as CLT
import           Control.Monad ( MonadPlus, when )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.Monad.RWS.Strict as CMR
import qualified Data.ByteString as BS
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import qualified Data.Sequence as Seq
import qualified Data.Text as DT
import qualified Data.Text.Encoding as DTE
import qualified Data.Text.Encoding.Error as DTEE
import qualified Prettyprinter as PP
import           System.FilePath ( (</>) )
import           Text.Megaparsec ( (<?>) )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL

import qualified Language.Sleigh.AST as A

data Token = StringLiteral DT.Text
           | Number Int
           | Identifier A.Identifier
           -- Syntax
           | Colon
           | Assign
           | Comma
           | LParen
           | RParen
           | LBracket
           | RBracket
           | LBrace
           | RBrace
           | Semi
           | Amp
           -- Special identifiers
           | Token
           | Space
           | Define
           | Register
           | Signed
           | Context
           | Size
           | Offset
           | Attach
           | Variables
           | Is
           | Export
           | Macro
           deriving (Eq, Ord, Show)

instance PP.Pretty Token where
  pretty t =
    case t of
      StringLiteral s -> PP.viaShow s
      Number i -> PP.pretty i
      Identifier i -> PP.pretty i
      Colon -> ":"
      Assign -> "="
      Comma -> ","
      LParen -> "("
      RParen -> ")"
      LBracket -> "["
      RBracket -> "]"
      LBrace -> "{"
      RBrace -> "}"
      Semi -> ";"
      Amp -> "&"
      Token -> "token"
      Space -> "space"
      Define -> "define"
      Register -> "register"
      Signed -> "signed"
      Context -> "context"
      Size -> "size"
      Offset -> "offset"
      Attach -> "attach"
      Variables -> "variables"
      Is -> "is"
      Export -> "export"
      Macro -> "macro"

data Positioned a =
  WithPos { startPos :: TM.SourcePos
          , endPos :: TM.SourcePos
          , tokenLength :: Word
          , tokenVal :: a
          } deriving (Eq, Ord, Show)

instance (PP.Pretty a) => PP.Pretty (Positioned a) where
  pretty p = PP.hcat [ PP.pretty (tokenVal p)
                     , "@"
                     , PP.pretty (TM.sourcePosPretty (startPos p))
                     , "-"
                     , PP.pretty (TM.sourcePosPretty (endPos p))
                     ]

{- Note [Conditional Compilation Design]

Sleigh supports conditional compilation much like the C preprocessor. It emits
tokens when the current conditional "compilation" context is true (and discards
tokens otherwise).

To support this efficiently, we will maintain a stack of conditions that are
active, with the empty stack corresponding to the top level.

- @if X and @ifdef X push a condition test onto the stack (which can be evaluated to True or False)
- @elif X inspects the current element on the stack and replaces the top element with
  - AlwaysFalse if the previous element was True or AlwaysFalse
  - True if X is True
  - False if X is False
- @else inspects the current top of the stack and replaces it with
  - True if the top is False
  - False otherwise
- @endif pops the stack

At every token, evaluate the conditions on the stack and only emit the token if
the stack evaluates to True. Note that we can cache the evaluation for efficiency

-}

data ConditionalGuard where
  IsDefined :: A.Identifier -> ConditionalGuard

data ConditionKind = AlwaysFalseK | FalseK | TrueK

type AlwaysFalseK = 'AlwaysFalseK
type FalseK = 'FalseK
type TrueK = 'TrueK

data Condition k where
  -- | The state for a level of conditional can enter the AlwaysFalse state only
  -- after it has been True already (so we require a witness to that fact to
  -- make an AlwaysFalse)
  AlwaysFalse :: Condition TrueK -> Condition AlwaysFalseK
  -- | The state can be CurrentlyTrue if we are under a branch with a condition
  -- that evaluates to True (or an Else when no other branches evaluated to
  -- True). There is no convenient witness to this
  CurrentlyTrue :: Maybe ConditionalGuard -> Condition TrueK
  CurrentlyFalse :: Maybe ConditionalGuard -> Condition FalseK

data SomeCondition where
  SomeCondition :: Condition k -> SomeCondition

evaluateCondition :: [SomeCondition] -> Bool
evaluateCondition = F.foldl' evalOne True
  where
    evalOne :: Bool -> SomeCondition -> Bool
    evalOne acc (SomeCondition c) =
      case c of
        AlwaysFalse _ -> False
        CurrentlyTrue _ -> acc
        CurrentlyFalse _ -> False

data ParserState =
  ParserState { _parserDefines :: Map.Map A.Identifier Token
              -- ^ Current preprocessor definitions
              --
              -- Note that these are added and removed during parsing
              , _accumulatedTokens :: Seq.Seq (Positioned Token)
              -- ^ Preprocessed tokens
              , _conditionalStack :: (Bool, [SomeCondition])
              -- ^ The stack of conditional compilation conditions in scope,
              -- along with the cached evaluation of the stack value
              --
              -- These are together to ensure that they are always updated
              -- together
              }

$(CLT.makeLenses ''ParserState)

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

data IncludeContext =
  IncludeContext { fileIssuingInclude :: FilePath
                   -- ^ The file containing the include statement
                 , includePos :: TM.SourcePos
                   -- ^ The line number in the including file that the include occurs on
                 }
  deriving (Eq, Ord, Show)

instance PP.Pretty IncludeContext where
  pretty ic = PP.pretty (fileIssuingInclude ic) <> ":" <> PP.pretty (TM.sourcePosPretty (includePos ic))

data SleighPreprocessingError = ErrorInIncludedFile [IncludeContext] WrappedErrorBundle
                              | UndefinedMacroExpansion DT.Text
                              | UnmatchedElse
                              | UnmatchedEndif
  deriving (Eq, Ord, Show)

newtype WrappedErrorBundle = WrappedErrorBundle (TM.ParseErrorBundle DT.Text SleighPreprocessingError)
  deriving (Eq, Show)

instance Ord WrappedErrorBundle where
  compare (WrappedErrorBundle b1) (WrappedErrorBundle b2) =
    compare (TM.errorBundlePretty b1) (TM.errorBundlePretty b2)

instance TM.ShowErrorComponent SleighPreprocessingError where
  showErrorComponent e =
    case e of
      ErrorInIncludedFile ctxs (WrappedErrorBundle err) ->
        show $ PP.vcat [ "Error in included file:"
                       , PP.indent 2 (PP.vcat [PP.pretty ic | ic <- ctxs])
                       , PP.pretty (TM.errorBundlePretty err)
                       ]
      UndefinedMacroExpansion name -> "Undefined macro expansion: " <> DT.unpack name
      UnmatchedElse -> "Unmatched `@else`"
      UnmatchedEndif -> "Unmatched `@endif`"

newtype PP a = PP { unPP :: TM.ParsecT SleighPreprocessingError DT.Text (CMR.RWST ParserEnv () ParserState IO) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , MonadIO
           , TM.MonadParsec SleighPreprocessingError DT.Text
           , CMR.MonadReader ParserEnv
           , CMR.MonadState ParserState
           )

spaceConsumer :: PP ()
spaceConsumer = TMCL.space TMC.space1 lineComment blockComment
  where
    lineComment = TMCL.skipLineComment "#"
    blockComment = empty

lexeme :: PP a -> PP a
lexeme = TMCL.lexeme spaceConsumer

token :: (a -> Token) -> PP a -> PP (Positioned Token)
token con p = do
  spos <- TM.getSourcePos
  o1 <- TM.getOffset
  a <- p
  epos <- TM.getSourcePos
  o2 <- TM.getOffset
  let tk = WithPos { startPos = spos
                   , endPos = epos
                   , tokenLength = fromIntegral (o2 - o1)
                   , tokenVal = con a
                   }
  return $! tk

-- | A simple token with no data payload
stoken :: Token -> DT.Text -> PP (Positioned Token)
stoken tk txt = token (const tk) (TMC.string txt >> return ())

-- | Parse an identifier
identifier :: PP A.Identifier
identifier = do
  c1 <- identSymbols <|> TMC.letterChar
  cs <- TM.many (TMC.alphaNumChar <|> identSymbols)
  return (A.Identifier (DT.pack (c1 : cs)))
  where
    identSymbols = TM.satisfy (\c -> c == '_' || c == '.')

macroExpansion :: PP (Positioned Token)
macroExpansion = do
  spos <- TM.getSourcePos
  o1 <- TM.getOffset
  _ <- lexeme (TMC.string "$(")
  ident <- identifier
  _ <- lexeme (TMC.char ')')
  epos <- TM.getSourcePos
  o2 <- TM.getOffset

  defs <- CL.use parserDefines
  case Map.lookup ident defs of
    Just expansion -> do
      let tk = WithPos { startPos = spos
                       , endPos = epos
                       , tokenLength = fromIntegral (o2 - o1)
                       , tokenVal = expansion
                       }
      return $! tk
    Nothing -> TM.customFailure (UndefinedMacroExpansion (A.identifierText ident))

-- | Parse a string literal
--
-- Note that there does not seem to be an escape syntax according to the Sleigh documentation
--
-- Does not include the double quotes
stringLiteral :: PP DT.Text
stringLiteral = do
  _ <- TMC.char '"'
  s <- TM.many (TM.anySingleBut '"')
  _ <- TMC.char '"'
  return (DT.pack s)

-- | Parse any token that is to be preserved for the actual parsing process
--
-- These are accumulated in the state, rather than returned
anyToken :: PP ()
anyToken = do
  t <- TM.choice [ TM.try (token Identifier identifier)
                 , TM.try (token StringLiteral stringLiteral)
                 , TM.try (token Number TMCL.decimal)
                 , TM.try (token Number (TMC.string "0x" *> TMCL.hexadecimal))
                 , TM.try (stoken Colon ":")
                 , TM.try (stoken Assign "=")
                 , TM.try (stoken LParen "(")
                 , TM.try (stoken RParen ")")
                 , TM.try (stoken LBracket "[")
                 , TM.try (stoken RBracket "]")
                 , TM.try (stoken LBrace "{")
                 , TM.try (stoken RBrace "}")
                 , TM.try (stoken Comma ",")
                 , TM.try (stoken Semi ";")
                 , TM.try (stoken Amp "&")
                 , TM.try (stoken Define "define")
                 , TM.try (stoken Space "space")
                 , TM.try (stoken Register "register")
                 , TM.try (stoken Signed "signed")
                 , TM.try (stoken Context "context")
                 , TM.try (stoken Size "size")
                 , TM.try (stoken Offset "offset")
                 , TM.try (stoken Attach "attach")
                 , TM.try (stoken Variables "variables")
                 , TM.try (stoken Is "is")
                 , TM.try (stoken Export "export")
                 , TM.try (stoken Macro "macro")
                 , TM.try macroExpansion
                 ]
  emitToken <- CL.use (conditionalStack . CL._1)
  when emitToken $ accumulatedTokens %= (Seq.|> t)


parseComment :: PP ()
parseComment = do
  _ <- TMC.char '#'
  _ <- TM.manyTill TM.anySingle TMC.eol
  return ()

-- | @\@define IDENT val@
parsePreprocessorDefine :: PP ()
parsePreprocessorDefine = do
  _ <- lexeme (TMC.string "define")
  ident <- lexeme identifier <?> "Preprocessor definition identifier"
  -- The definition can be either a bare identifier or a quoted literal; we have
  -- to unwrap the type wrapper from identifiers
  let asLit = StringLiteral <$> stringLiteral
  let asIdent = Identifier <$> identifier
  value <- lexeme (TM.try asLit <|> asIdent) <?> "Preprocessor definition value"
  parserDefines CL.%= Map.insert ident value

-- | @\@undef IDENT@
parsePreprocessorUndefine :: PP ()
parsePreprocessorUndefine = do
  _ <- lexeme (TMC.string "undef")
  ident <- lexeme identifier
  parserDefines CL.%= Map.delete ident

-- | @\@ifdef IDENT@
parsePreprocessorIfdef :: PP ()
parsePreprocessorIfdef = do
  _ <- lexeme (TMC.string "ifdef")
  ident <- lexeme identifier
  defs <- CL.use parserDefines
  case Map.lookup ident defs of
    Nothing -> do
      stk <- CL.use (conditionalStack . CL._2)
      let stk' = SomeCondition (CurrentlyFalse (Just (IsDefined ident))) : stk
      conditionalStack .= (evaluateCondition stk', stk')
    Just _ -> do
      stk <- CL.use (conditionalStack . CL._2)
      let stk' = SomeCondition (CurrentlyTrue (Just (IsDefined ident))) : stk
      conditionalStack .= (evaluateCondition stk', stk')

-- | @\@endif@
parsePreprocessorEndif :: PP ()
parsePreprocessorEndif = do
  _ <- lexeme (TMC.string "endif")
  stk <- CL.use (conditionalStack . CL._2)
  case stk of
    [] -> TM.customFailure UnmatchedEndif
    _ : rest -> conditionalStack .= (evaluateCondition rest, rest)

parsePreprocessorElse :: PP ()
parsePreprocessorElse = do
  _ <- lexeme (TMC.string "else")
  stk <- CL.use (conditionalStack . CL._2)
  case stk of
    [] -> TM.customFailure UnmatchedElse
    SomeCondition top : rest -> do
      let next = case top of
            AlwaysFalse {} -> SomeCondition top
            CurrentlyTrue {} -> SomeCondition (CurrentlyFalse Nothing)
            CurrentlyFalse {} -> SomeCondition (CurrentlyTrue Nothing)
      let stk' = next : rest
      conditionalStack .= (evaluateCondition stk', stk')

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
  -> PP ()
  -> PP ()
processInclude filename parseWith = do
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

  (res, nextState, _) <- liftIO $ CMR.runRWST (TM.runParserT (unPP parseWith) (DT.unpack filename) txt) env' currentState
  CMR.put nextState
  case res of
    Right () -> return ()
    Left errs -> do
      ctxts <- CMR.asks contextStack
      TM.customFailure (ErrorInIncludedFile ctxts (WrappedErrorBundle errs))

parsePreprocessorInclude :: PP ()
parsePreprocessorInclude = do
  _ <- lexeme (TMC.string "include")
  filename <- lexeme stringLiteral
  processInclude filename (TM.some topLevel >> TM.eof)

-- | Parse and process preprocessor directives
--
-- Note that when this has been called, the @\@@ sign has already been processed by 'topLevel'
--
-- Supported preprocessor directives are:
--
--  * @\@undef@
--  * @\@define@
--  * @\@include@
--  * @\@if, \@ifdef, \@ifndef, \@else, \@elif, \@endif@
parsePreprocessorDirective :: PP ()
parsePreprocessorDirective = do
  TM.choice [ TM.try parsePreprocessorDefine
            , TM.try parsePreprocessorUndefine
            , TM.try parsePreprocessorIfdef
            , TM.try parsePreprocessorEndif
            , TM.try parsePreprocessorElse
            -- Note that this needs to come last so that errors in included
            -- files can be propagated properly; also it must not be wrapped in
            -- a 'TM.try'
            , parsePreprocessorInclude
            ]

topLevel :: PP ()
topLevel =
  TM.choice [ TM.try anyToken
            , TM.try parseComment
            , TM.try (TM.some TMC.space1 >> return ())
            , TMC.char '@' *> parsePreprocessorDirective
            ]

parsePreprocess :: PP ()
parsePreprocess = do
  _ <- TM.some topLevel
  TM.eof

preprocessSleigh
  :: FilePath
  -- ^ The path to resolve includes relative to
  -> FilePath
  -- ^ The filename of the sleigh file
  -> BS.ByteString
  -- ^ The contents of the sleigh file to parse
  -> IO (Either (TM.ParseErrorBundle DT.Text SleighPreprocessingError) (Seq.Seq (Positioned Token)))
preprocessSleigh includePath filename contents = do
  (eres, s1, _) <- CMR.runRWST (TM.runParserT (unPP parsePreprocess) filename txt) env s0
  case eres of
    Left err -> return (Left err)
    _ -> return (Right (_accumulatedTokens s1))
  where
    txt = DTE.decodeUtf8With DTEE.lenientDecode contents
    env = ParserEnv { parserWorkingDirectory = includePath
                    , contextStack = []
                    , thisFile = filename
                    }
    s0 = ParserState { _parserDefines = Map.empty
                     , _accumulatedTokens = mempty
                     , _conditionalStack = (True, [])
                     }
