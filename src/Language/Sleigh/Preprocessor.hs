{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
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
  , SleighPreprocessingError(..)
  ) where

import           Control.Applicative ( Alternative, (<|>), empty )
import           Control.Lens ( (%=), (.=) )
import qualified Control.Lens as CL
import qualified Control.Lens.TH as CLT
import           Control.Monad ( MonadPlus, when )
import           Control.Monad.IO.Class ( MonadIO, liftIO )
import qualified Control.Monad.RWS.Strict as CMR
import qualified Data.ByteString as BS
import qualified Data.Char as Char
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

import qualified Language.Sleigh.Identifier as I
import           Language.Sleigh.Token

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
  IsDefined :: I.Identifier -> ConditionalGuard
  Conjunction :: ConditionalGuard -> ConditionalGuard -> ConditionalGuard
  StringEqual :: DT.Text -> DT.Text -> ConditionalGuard

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
  ParserState { _parserDefines :: Map.Map I.Identifier Token
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

-- | A simple textual token with no data payload
--
-- This is for non-symbol tokens, and has additional logic using
-- 'TM.notFollowedBy' to ensure that tokens embedded in longer identifiers are
-- not slurped up by the maximum munch rule
textToken :: Token -> DT.Text -> PP (Positioned Token)
textToken tk txt = do
  t <- token (const tk) (TMC.string txt >> return ())
  TM.notFollowedBy (TM.satisfy isIdentifierChar)
  return t

-- | Returns true if the character is a valid character in the body of an
-- identifier (i.e., not the first character)
isIdentifierChar :: Char -> Bool
isIdentifierChar c = Char.isAlphaNum c || isIdentSymbol c

isIdentSymbol :: Char -> Bool
isIdentSymbol c = c == '_' || c =='.'

-- | Parse an identifier
identifier :: PP I.Identifier
identifier = do
  c1 <- TM.satisfy isIdentSymbol <|> TMC.letterChar
  cs <- TM.many (TM.satisfy isIdentifierChar)
  return (I.Identifier (DT.pack (c1 : cs)))

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
    Nothing -> TM.customFailure (UndefinedMacroExpansion (I.identifierText ident))

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
  t <- TM.choice [ TM.try (stoken SignedGreaterEquals "s>=")
                 , TM.try (stoken SignedShiftRight "s>>")
                 , TM.try (stoken SignedGreaterThan "s>")
                 , TM.try (stoken SignedLessEquals "s<=")
                 , TM.try (stoken SignedLessThan "s<")
                 , TM.try (stoken SDiv "s/")
                 , TM.try (stoken SMod "s%")
                 , TM.try (stoken FAdd "f+")
                 , TM.try (stoken FDiv "f/")
                 , TM.try (stoken FMul "f*")
                 , TM.try (stoken FSub "f-")
                 , TM.try (stoken FEQ "f==")
                 , TM.try (stoken FLE "f<=")
                 , TM.try (stoken FLT "f<")
                 , TM.try (stoken FGE "f>=")
                 , TM.try (stoken FGT "f>")
                 , TM.try (stoken FNE "f!=")
                 , TM.try (token StringLiteral stringLiteral)
                 , TM.try (token Number (TMC.string "0x" *> TMCL.hexadecimal))
                 , TM.try (token Number (TMC.string "0b" *> TMCL.binary))
                 , TM.try (token Number TMCL.decimal)
                 , TM.try (stoken Amp "$and") -- See Note [Alternative Bitwise Operators]
                 , TM.try (stoken Amp "$or") -- See Note [Alternative Bitwise Operators]
                 , TM.try macroExpansion
                 , TM.try (stoken BitwiseNot "~")
                 , TM.try (stoken ShiftLeft "<<")
                 , TM.try (stoken ShiftRight ">>")
                 , TM.try (stoken GreaterEquals ">=")
                 , TM.try (stoken GreaterThan ">")
                 , TM.try (stoken LessEquals "<=")
                 , TM.try (stoken LessThan "<")
                 , TM.try (stoken Colon ":")
                 , TM.try (stoken Equals "==")
                 , TM.try (stoken NotEquals "!=")
                 , TM.try (stoken Assign "=")
                 , TM.try (stoken LParen "(")
                 , TM.try (stoken RParen ")")
                 , TM.try (stoken LBracket "[")
                 , TM.try (stoken RBracket "]")
                 , TM.try (stoken LBrace "{")
                 , TM.try (stoken RBrace "}")
                 , TM.try (stoken Comma ",")
                 , TM.try (stoken Semi ";")
                 , TM.try (stoken LogicalAnd "&&")
                 , TM.try (stoken LogicalOr "||")
                 , TM.try (stoken BitwiseOr "|")
                 , TM.try (stoken Caret "^")
                 , TM.try (stoken Amp "&")
                 , TM.try (stoken Dollar "$")
                 , TM.try (stoken Div "/")
                 , TM.try (stoken Mod "%")
                 , TM.try (stoken Asterisk "*")
                 , TM.try (stoken Plus "+")
                 , TM.try (stoken Minus "-")
                 , TM.try (stoken Exclamation "!")
                 , TM.try (textToken Define "define")
                 , TM.try (textToken Attach "attach")
                 , TM.try (textToken Variables "variables")
                 , TM.try (textToken Export "export")
                 , TM.try (textToken Macro "macro")
                 , TM.try (textToken Is "is")
                 , TM.try (token Ident identifier)
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
  let asIdent = Ident <$> identifier
  value <- lexeme (TM.try asLit <|> asIdent) <?> "Preprocessor definition value"
  parserDefines CL.%= Map.insert ident value

-- | @\@undef IDENT@
parsePreprocessorUndefine :: PP ()
parsePreprocessorUndefine = do
  _ <- lexeme (TMC.string "undef")
  ident <- lexeme identifier
  parserDefines CL.%= Map.delete ident

-- | Parse an operand for a preprocessor binary operator, which must be a string
preprocessorBinaryOperand :: PP DT.Text
preprocessorBinaryOperand = do
  let asStringLit = lexeme stringLiteral
  let fromIdent = resolveMacro =<< lexeme identifier
  TM.try asStringLit <|> fromIdent
  where
    resolveMacro ident = do
      defs <- CL.use parserDefines
      case Map.lookup ident defs of
        Just (StringLiteral l) -> return l
        _ -> TM.customFailure (UndefinedMacroExpansion (I.identifierText ident))

preprocessorEquality :: PP ConditionalGuard
preprocessorEquality = do
  s1 <- preprocessorBinaryOperand
  _ <- lexeme (TMC.string "==")
  s2 <- preprocessorBinaryOperand
  return (StringEqual s1 s2)

preprocessorIsDefined :: PP ConditionalGuard
preprocessorIsDefined = do
  _ <- lexeme (TMC.string "defined")
  _ <- lexeme (TMC.char '(')
  ident <- lexeme identifier
  _ <- lexeme (TMC.char ')')
  return (IsDefined ident)

preprocessorAtomicExpression :: PP ConditionalGuard
preprocessorAtomicExpression =
  TM.choice [ TM.try preprocessorIsDefined
            , TM.try preprocessorEquality
            , lexeme (TMC.char '(') *> preprocessorExpression <* lexeme (TMC.char ')')
            ]

preprocessorConj :: PP ConditionalGuard
preprocessorConj =
  TM.choice [ TM.try conj
            , preprocessorAtomicExpression
            ]
  where
    conj = do
      e1 <- preprocessorAtomicExpression
      _ <- lexeme (TMC.string "&&")
      e2 <- preprocessorConj
      return (Conjunction e1 e2)


preprocessorExpression :: PP ConditionalGuard
preprocessorExpression = preprocessorConj

evaluateConditionalGuard :: ConditionalGuard -> PP Bool
evaluateConditionalGuard cg =
  case cg of
    IsDefined ident -> do
      defs <- CL.use parserDefines
      return $ maybe False (const True) (Map.lookup ident defs)
    StringEqual s1 s2 -> return (s1 == s2)
    Conjunction cg1 cg2 ->
      (&&) <$> evaluateConditionalGuard cg1 <*> evaluateConditionalGuard cg2

-- | @\@if EXPR@
parsePreprocessorIf :: PP ()
parsePreprocessorIf = do
  _ <- lexeme (TMC.string "if")
  expr <- preprocessorExpression
  exprVal <- evaluateConditionalGuard expr
  stk <- CL.use (conditionalStack . CL._2)
  case exprVal of
    True -> do
      let stk' = SomeCondition (CurrentlyTrue (Just expr)) : stk
      conditionalStack .= (evaluateCondition stk', stk')
    False -> do
      let stk' = SomeCondition (CurrentlyFalse (Just expr)) : stk
      conditionalStack .= (evaluateCondition stk', stk')

-- | @\@ifdef IDENT@
parsePreprocessorIfdef :: PP ()
parsePreprocessorIfdef = do
  _ <- lexeme (TMC.string "ifdef")
  ident <- lexeme identifier
  stk <- CL.use (conditionalStack . CL._2)
  let condGuard = IsDefined ident
  exprVal <- evaluateConditionalGuard condGuard
  case exprVal of
    True -> do
      let stk' = SomeCondition (CurrentlyTrue (Just condGuard)) : stk
      conditionalStack .= (evaluateCondition stk', stk')
    False -> do
      let stk' = SomeCondition (CurrentlyFalse (Just condGuard)) : stk
      conditionalStack .= (evaluateCondition stk', stk')

-- | @\@ifndef IDENT@
parsePreprocessorIfndef :: PP ()
parsePreprocessorIfndef = do
  _ <- lexeme (TMC.string "ifndef")
  ident <- lexeme identifier
  stk <- CL.use (conditionalStack . CL._2)
  let condGuard = IsDefined ident
  exprVal <- evaluateConditionalGuard condGuard
  case exprVal of
    True -> do
      let stk' = SomeCondition (CurrentlyFalse (Just condGuard)) : stk
      conditionalStack .= (evaluateCondition stk', stk')
    False -> do
      let stk' = SomeCondition (CurrentlyTrue (Just condGuard)) : stk
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
            , TM.try parsePreprocessorIfndef
            , TM.try parsePreprocessorIf
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

{- Note [Alternative Bitwise Operators]

The Ghidra manual notes:

> Note that there are two syntax forms for the logical operators in a pattern
> expression. When an expression is used as part of a constraint, the “$and” and
> “$or” forms of the operators must be used in order to distinguish the bitwise
> operators from the special pattern combining operators, ‘&’ and ‘|’ (as
> described in Section 7.4.2, “The '&' and '|' Operators”). However inside the
> square braces of the disassembly action section, ‘&’ and ‘|’ are interpreted
> as the usual logical operators.

In some contexts, the & symbol has additional meaning and the alternative form
is required to avoid syntactic ambiguity.  We parse them the same way as we
parse the normal operator and do not distinguish while parsing.

The lexing rule must come before the macro expansion rule, otherwise it would
attempt macro expansion and fail.

The treatment in the parser would be incorrect if someone attempted to use
`$and` in another context as a CPP macro. We assume that will not happen.

-}
