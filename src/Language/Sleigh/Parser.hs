{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Sleigh.Parser (
  sleighParser
  ) where

import           Control.Applicative ( (<|>) )
import           Control.Monad ( MonadPlus )
import qualified Data.List.NonEmpty as DLN
import qualified Data.Text as DT
import qualified Data.Text.Read as DTR
import           Text.Megaparsec ( (<?>) )
import qualified Text.Megaparsec as TM

import           Language.Sleigh.AST
import           Language.Sleigh.Identifier as I
import qualified Language.Sleigh.ParserMonad as P
import qualified Language.Sleigh.Preprocessor as PP

-- | A parser for a non-empty sequence of values
--
-- Basically the same as 'TM.some', but reflecting the required multiplicity at
-- the type level
nonEmptyList :: (MonadPlus m) => m a -> m (DLN.NonEmpty a)
nonEmptyList p = do
  v0 <- p
  vs <- TM.many p
  return (v0 DLN.:| vs)

-- | Parse a non-empty bracketed list, where the brackets are optional for a
-- singleton list
nonEmptyBracketedList :: P.SleighM a -> P.SleighM (DLN.NonEmpty a)
nonEmptyBracketedList p =
  TM.choice [ TM.try (token PP.LBracket *> nonEmptyList p <* token PP.RBracket)
            , (DLN.:|[]) <$> p
            ]

-- | Parse an expected token
token :: PP.Token -> P.SleighM ()
token t = TM.satisfy ((== t) . PP.tokenVal) >> return ()

-- | Parse an expected identifier (like a token, but for which no specific token is identified)
--
-- This is useful for tokens that appear in keyword-like positions, but that are
-- not reserved words in sleigh (or are not obviously reserved words)
tokenIdentifier :: String -> P.SleighM ()
tokenIdentifier t = TM.satisfy asIdent >> return ()
  where
    asIdent tk =
      case PP.tokenVal tk of
        PP.Ident i -> I.identifierText i == DT.pack t
        _ -> False

parseString :: P.SleighM DT.Text
parseString = TM.try $ do
  tk <- TM.anySingle <?> "String literal"
  case tk of
    PP.WithPos { PP.tokenVal = PP.StringLiteral t } -> return t
    _ -> TM.empty

parseIdentifier :: P.SleighM Identifier
parseIdentifier = TM.try $ do
  tk <- TM.anySingle <?> "Identifier"
  case tk of
    PP.WithPos { PP.tokenVal = PP.Ident i } -> return i
    _ -> TM.empty

parseNumber :: P.SleighM Int
parseNumber = TM.try $ do
  -- NOTE: this can also be a string literal token that needs to be parsed as a number, due to preprocessor macro expansions
  tk <- TM.anySingle <?> "Number"
  case tk of
    PP.WithPos { PP.tokenVal = PP.Number i } -> return i
    PP.WithPos { PP.tokenVal = PP.StringLiteral t }
      | Right (n, "") <- DTR.decimal t -> return n
    _ -> TM.empty

parseWord :: P.SleighM Word
parseWord = fromIntegral <$> parseNumber

parseEndian :: P.SleighM Definition
parseEndian = do
  tokenIdentifier "endian"
  token PP.Assign
  end <- parseString
  case end of
    "little" -> return (DefEndianness Little)
    "big" -> return (DefEndianness Big)
    _ -> TM.customFailure (P.InvalidEndianness end)

parseSpaceType :: P.SleighM SpaceType
parseSpaceType = TM.try (tokenIdentifier "ram_space" *> pure Memory) <|> (tokenIdentifier "register_space" *> pure Register)

parseDefault :: P.SleighM Default
parseDefault = TM.try (tokenIdentifier "default" *> pure IsDefault) <|> pure NotDefault

parseInstructionAlignment :: P.SleighM Definition
parseInstructionAlignment = do
  tokenIdentifier "alignment"
  token PP.Assign
  numBytes <- parseNumber
  return (DefInstructionAlignment (fromIntegral numBytes))

parseRegisterBank :: P.SleighM Definition
parseRegisterBank = do
  tokenIdentifier "register"
  tokenIdentifier "offset"
  token PP.Assign
  off <- parseNumber
  tokenIdentifier "size"
  token PP.Assign
  regSize <- parseNumber
  idents <- TM.try parseList  <|> parseSingleton
  return (DefRegisterBank (fromIntegral off) (fromIntegral regSize) idents)
  where
    parseSingleton = (:[]) <$> parseIdentifier
    parseList = token PP.LBracket *> TM.many parseIdentifier <* token PP.RBracket

parseSpace :: P.SleighM Definition
parseSpace = do
  tokenIdentifier "space"
  ident <- parseIdentifier
  tokenIdentifier "type"
  token PP.Assign
  ty <- parseSpaceType
  tokenIdentifier "size"
  token PP.Assign
  addrSize <- parseNumber
  dflt <- parseDefault
  return (DefSpace ident ty (fromIntegral addrSize) dflt)

parseNormalAttr :: P.SleighM Attribute
parseNormalAttr =
  TM.choice [ TM.try (tokenIdentifier "signed" *> pure Signed)
            , TM.try (tokenIdentifier "dec" *> pure Decimal)
            , TM.try (tokenIdentifier "hex" *> pure Hexadecimal)
            ]

parseContextVariables :: P.SleighM Definition
parseContextVariables = do
  tokenIdentifier "context"
  ident <- parseIdentifier
  fields <- TM.many parseContextField
  return (DefContextVariables ident fields)
  where
    parseContextAttr = TM.try (tokenIdentifier "noflow" *> pure NoFlow) <|> (NormalAttribute <$> parseNormalAttr)
    parseContextField = do
      ident <- parseIdentifier
      token PP.Assign
      token PP.LParen
      lo <- parseNumber
      token PP.Comma
      hi <- parseNumber
      token PP.RParen
      attrs <- TM.many parseContextAttr
      return (ContextField ident (fromIntegral lo) (fromIntegral hi) attrs)

parseTokens :: P.SleighM Definition
parseTokens = do
  tokenIdentifier "token"
  ident <- parseIdentifier
  token PP.LParen
  numBits <- parseNumber
  token PP.RParen
  fields <- TM.many parseField
  return (DefTokenFields ident (fromIntegral numBits) fields)
  where
    parseField = do
      name <- parseIdentifier
      token PP.Assign
      token PP.LParen
      lo <- parseNumber
      token PP.Comma
      hi <- parseNumber
      token PP.RParen
      attrs <- TM.many parseNormalAttr
      return (TokenField name (fromIntegral lo) (fromIntegral hi) attrs)

-- | Parse an interpretation for an @attach variables@ context
--
-- Note that we want to treat @_@ specially, as it denotes an invalid
-- interpretation. Unfortunately, it is also a valid identifier. We just parse
-- an identifier and case on it.
parseValueInterpretation :: P.SleighM ValueInterpretation
parseValueInterpretation = do
  iden <- parseIdentifier
  case identifierText iden of
    "_" -> return InvalidInterpretation
    _ -> return (ValidInterpretation iden)

parseAttachVariables :: P.SleighM Attach
parseAttachVariables = do
  token PP.Variables

  fieldlist <- nonEmptyBracketedList parseIdentifier

  token PP.LBracket
  registerlist <- TM.many parseValueInterpretation
  token PP.RBracket

  return $! AttachVariables fieldlist registerlist

parseAttachNames :: P.SleighM Attach
parseAttachNames = do
  tokenIdentifier "names"

  fieldList <- nonEmptyBracketedList parseIdentifier

  token PP.LBracket
  names <- TM.many parseString
  token PP.RBracket

  return $! AttachNames fieldList names

parseAttach :: P.SleighM ()
parseAttach = do
  token PP.Attach
  attach <- TM.choice [ TM.try parseAttachVariables
                      , TM.try parseAttachNames
                      ]
  token PP.Semi
  P.recordAttach attach

parseBitPattern :: P.SleighM BitPattern
parseBitPattern = parseBitDisj
  where
    parseBitDisj = TM.choice [ TM.try disj, parseBitConj ]
    parseBitConj = TM.choice [ TM.try conj, parseAtomicBitPattern ]
    disj = do
      lhs <- parseBitConj
      token PP.BitwiseOr
      rhs <- parseBitPattern
      return (Or lhs rhs)
    conj = do
      lhs <- parseAtomicBitPattern
      token PP.Amp
      rhs <- parseBitPattern
      return (And lhs rhs)

    eqConstraint = do
      iden <- parseIdentifier
      token PP.Assign
      num <- parseNumber
      return (EqualityConstraint iden (fromIntegral num))
    ineqConstraint = do
      iden <- parseIdentifier
      token PP.NotEquals
      num <- parseNumber
      return (InequalityConstraint iden (fromIntegral num))

    parseAtomicBitPattern =
      TM.choice [ TM.try (Constraint <$> eqConstraint)
                , TM.try (Constraint <$> ineqConstraint)
                , TM.try ((Constraint . Unconstrained) <$> parseIdentifier)
                , TM.try ((Constraint . StringConstraint) <$> parseString)
                , TM.between (token PP.LParen) (token PP.RParen) parseBitPattern
                ]

-- | Parse a single expression
--
-- Note that it would be ideal to be able to use
-- @Control.Monad.Combinators.Expr@ from the parser-combinators library;
-- however, a few of the productions have irregular parses that don't quite fit
-- into the model of that module.
--
-- Also note that the manual look ahead and casing in this function (as opposed
-- to a more idiomatic nested 'TM.try' and 'TM.choice' structure) significantly
-- increases performance.
parseExpression :: P.SleighM Expr
parseExpression = parsePrec10
  where
    parsePrec10 = do
      lhs <- parsePrec9
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.BitwiseOr -> BitwiseOr <$> pure lhs <*> (token PP.BitwiseOr *> parseExpression)
        _ -> pure lhs
    parsePrec9 = do
      lhs <- parsePrec8
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.Caret -> BitwiseXor <$> pure lhs <*> (token PP.Caret *> parseExpression)
        _ -> pure lhs
    parsePrec8 = do
      lhs <- parsePrec7
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.Amp -> BitwiseAnd <$> pure lhs <*> (token PP.Amp *> parseExpression)
        _ -> pure lhs
    parsePrec7 = do
      lhs <- parsePrec6
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.Equals -> RelEquals <$> pure lhs <*> (token PP.Equals *> parseExpression)
        PP.NotEquals -> RelNotEquals <$> pure lhs <*> (token PP.NotEquals *> parseExpression)
        _ -> pure lhs
    parsePrec6 = do
      lhs <- parsePrec5
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.LessThan -> RelLT <$> pure lhs <*> (token PP.LessThan *> parseExpression)
        PP.LessEquals -> RelLE <$> pure lhs <*> (token PP.LessEquals *> parseExpression)
        PP.GreaterThan -> RelGT <$> pure lhs <*> (token PP.GreaterThan *> parseExpression)
        PP.GreaterEquals -> RelGE <$> pure lhs <*> (token PP.GreaterEquals *> parseExpression)
        PP.SignedLessThan -> RelSLT <$> pure lhs <*> (token PP.SignedLessThan *> parseExpression)
        PP.SignedLessEquals -> RelSLE <$> pure lhs <*> (token PP.SignedLessEquals *> parseExpression)
        PP.SignedGreaterThan -> RelSGT <$> pure lhs <*> (token PP.SignedGreaterThan *> parseExpression)
        PP.SignedGreaterEquals -> RelSGE <$> pure lhs <*> (token PP.SignedGreaterEquals *> parseExpression)
        _ -> pure lhs
    parsePrec5 = do
      lhs <- parsePrec4
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.ShiftLeft -> ShiftLeft <$> pure lhs <*> (token PP.ShiftLeft *> parseExpression)
        PP.ShiftRight -> ShiftRight <$> pure lhs <*> (token PP.ShiftRight *> parseExpression)
        PP.SignedShiftRight -> SignedShiftRight <$> pure lhs <*> (token PP.SignedShiftRight *> parseExpression)
        _ -> pure lhs
    parsePrec4 = do
      lhs <- parsePrec3
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.Plus -> Add <$> pure lhs <*> (token PP.Plus *> parseExpression)
        PP.Minus -> Sub <$> pure lhs <*> (token PP.Minus *> parseExpression)
        _ -> pure lhs
    parsePrec3 = do
      lhs <- parsePrec2
      next <- TM.lookAhead TM.anySingle
      case PP.tokenVal next of
        PP.Asterisk -> Mul <$> pure lhs <*> (token PP.Asterisk *> parseExpression)
        _ -> pure lhs
    parsePrec2 = TM.choice [ TM.try (BitNot <$> (token PP.BitwiseNot *> parsePrec1))
                           , TM.try (AddressOf <$> (token PP.Amp *> parsePrec1))
                           , TM.try (Truncate <$> parsePrec1 <*> (token PP.Colon *> parseWord))
                           , TM.try (Negate <$> (token PP.Minus *> parsePrec1))
                           , TM.try (DynamicRef <$> (token PP.Asterisk *> TM.optional (TM.between (token PP.LBracket) (token PP.RBracket) parseIdentifier)) <*> (token PP.Colon *> parseNumber) <*> parseIdentifier)
                           , TM.try (Dereference <$> (token PP.Asterisk *> parsePrec1))
                           , parsePrec1
                           ]
    parsePrec1 = TM.choice [ TM.try (Funcall <$> parseIdentifier <*> TM.between (token PP.LParen) (token PP.RParen) (TM.sepBy parseExpression (token PP.Comma)))
                           , TM.try (Ref <$> parseIdentifier)
                           , TM.try (Word_ <$> parseWord)
                           , TM.try (TM.between (token PP.LParen) (token PP.RParen) parseExpression)
                           , VarNodeRef <$> TM.between (token PP.LBracket) (token PP.RBracket) parseIdentifier
                           ]

-- | A parser for the dynamic clause in an export statement
--
-- > *[<space>]:size
parseDynamicExport :: P.SleighM DynamicExport
parseDynamicExport = do
  token PP.Asterisk
  spc <- parseAddrSpace
  token PP.Colon
  sz <- parseNumber
  return DynamicExport { dynamicAddressSpace = spc
                       , dynamicSize = sz
                       }
  where
    parseAddrSpace = TM.optional (TM.between (token PP.LBracket) (token PP.RBracket) parseIdentifier)

parseJumpTarget :: P.SleighM JumpTarget
parseJumpTarget =
  TM.choice [ TM.try (VarNodeTarget <$> TM.between (token PP.LBracket) (token PP.RBracket) parseIdentifier)
            , TM.try ((LocalLabel . Label) <$> TM.between (token PP.LessThan) (token PP.GreaterThan) parseIdentifier)
            , IdentifierTarget <$> parseIdentifier
            ]

parseSemantics :: P.SleighM [Stmt]
parseSemantics = TM.many parseStatement
  where
    parseStatementWithoutSemi =
      TM.choice [ TM.try parseExportStmt
                , TM.try (Goto <$> (tokenIdentifier "goto" *> parseJumpTarget))
                , TM.try (Return <$> (tokenIdentifier "return" *> parseJumpTarget))
                , TM.try (Call <$> (tokenIdentifier "call" *> parseJumpTarget))
                , TM.try (Build <$> (tokenIdentifier "build" *> parseIdentifier))
                , TM.try (Local <$> (tokenIdentifier "local" *> parseIdentifier <* token PP.Assign) <*> parseExpression)
                  -- , TM.try parseIfThenElse
                , TM.try parseSimpleIf
                , TM.try parseAssignStmt
                , TM.try (ExprStmt <$> parseExpression)
                ]
    parseLabelStmt = (LabelMarker . Label) <$> TM.between (token PP.LessThan) (token PP.GreaterThan) parseIdentifier
    parseStatement = do
      s <- (parseStatementWithoutSemi <* token PP.Semi) <|> parseLabelStmt
      return s
    parseExportStmt = do
      token PP.Export
      v <- TM.choice [ TM.try (ExportedDynamic <$> parseDynamicExport <*> parseIdentifier)
                     , TM.try (ExportedIdentifier <$> parseIdentifier)
                     , ExportedConstant <$> parseWord <*> (token PP.Colon >> parseWord)
                     ]
      return (Export v)
    parseAssignStmt = Assign <$> parseExpression <*> (token PP.Assign *> parseExpression)

    -- Parse a simple statement of the form:
    --
    -- > if cond stmt;
    --
    -- This doesn't easily fit the normal block form very well because of the
    -- semicolon placement, so make a simple rule for it.
    parseSimpleIf = do
      tokenIdentifier "if"
      cond <- parseExpression
      stmt <- parseStatementWithoutSemi
      return (If cond [stmt] [])

    -- parseBlock = TM.choice [ TM.try (TM.between (token PP.LBrace) (token PP.RBrace) (TM.many parseStatement))
    --                        , (:[]) <$> parseStatement
    --                        ]
    -- -- The else clause is optional, so permit that
    -- parseElse = TM.choice [ TM.try (tokenIdentifier "else" >> parseBlock), pure [] ]

    -- parseIfThenElse = do
    --   tokenIdentifier "if"
    --   cond <- parseExpression
    --   onTrue <- parseBlock
    --   onFalse <- parseElse
    --   return (If cond onTrue onFalse)

parseMacro :: P.SleighM ()
parseMacro = do
  token PP.Macro
  name <- parseIdentifier
  args <- TM.between (token PP.LParen) (token PP.RParen) (TM.sepBy parseIdentifier (token PP.Comma))
  stmts <- TM.between (token PP.LBrace) (token PP.RBrace) parseSemantics
  let m = Macro { macroName = name
                , macroArguments = args
                , macroStatements = stmts
                }
  P.recordMacro m

parseConstructor :: P.SleighM ()
parseConstructor = do
  header <- parseTableHeader
  -- Note that manyTill consumes the end token, which is fine in this context
  dispTokens <- TM.manyTill TM.anySingle (token PP.Is)
  bp <- parseBitPattern
  disActionStmts <- TM.choice [ TM.try (TM.between (token PP.LBracket) (token PP.RBracket) parseSemantics)
                              , pure []
                              ]
  semStmts <- TM.between (token PP.LBrace) (token PP.RBrace) parseSemantics
  let con = Constructor { tableHeader = header
                        , displaySection = dispTokens
                        , bitPatterns = bp
                        , disassemblyActions = disActionStmts
                        , constructorStatements = semStmts
                        }
  P.recordConstructor con
  where
    parseTableHeader =
      TM.choice [ TM.try (token PP.Colon >> pure Root)
                , (Table <$> parseIdentifier <* token PP.Colon)
                ]

parsePCodeOp :: P.SleighM Definition
parsePCodeOp = do
  tokenIdentifier "pcodeop"
  name <- parseIdentifier
  return (DefPCodeOp name)

parseDefinition :: P.SleighM ()
parseDefinition = do
  token PP.Define
  -- Note that we try to parse endianness and alignment last, as they should
  -- only occur once. We try to parse the most common cases earlier
  def <- TM.choice [ TM.try parseRegisterBank
                   , TM.try parseContextVariables
                   , TM.try parseSpace
                   , TM.try parseTokens
                   , TM.try parseEndian
                   , TM.try parseInstructionAlignment
                   , TM.try parsePCodeOp
                   ]
  token PP.Semi
  P.recordDefinition def

-- | Parse a top-level entity
--
-- Each top-level entity is recorded and/or processed as a mutation of the parser state
topLevel :: P.SleighM ()
topLevel =
  TM.choice [ TM.try parseDefinition
            , TM.try parseConstructor
            , TM.try parseAttach
            , TM.try parseMacro
            ]

-- | The top-level parser for Sleigh files
--
-- Sleigh is a relatively complex language. Instead of a direct representation
-- of the AST, we want to provide a raw AST parser complemented with a semantic
-- analysis pass to extract a more useful representation later.
--
-- Because the parsing is stateful *and* the language requires definitions
-- before uses, we sink some of the simpler semantic information into the
-- parser.
sleighParser :: P.SleighM Sleigh
sleighParser = do
  _ <- TM.some topLevel
  TM.eof
  defs <- P.definitions
  atts <- P.attachments
  cons <- P.constructors
  macs <- P.macros
  return Sleigh { definitions = defs
                , attachments = atts
                , constructors = cons
                , macros = macs
                }
