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
        PP.Identifier i -> I.identifierText i == DT.pack t
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
    PP.WithPos { PP.tokenVal = PP.Identifier i } -> return i
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
parseBitPattern = parseBitConj
  where
    parseBitConj = TM.choice [ TM.try conj, parseAtomicBitPattern ]
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
    parseAtomicBitPattern =
      Constraint <$> TM.choice [ TM.try eqConstraint
                               , Unconstrained <$> parseIdentifier
                               ]

parseExpression :: P.SleighM Expr
parseExpression = parsePrec5
  where
    parsePrec5 = TM.choice [ TM.try (ShiftLeft <$> parsePrec4 <*> (token PP.ShiftLeft *> parseExpression))
                           , TM.try (ShiftRight <$> parsePrec4 <*> (token PP.ShiftRight *> parseExpression))
                           , parsePrec4
                           ]
    parsePrec4 = TM.choice [ TM.try (Add <$> parsePrec3 <*> (token PP.Plus *> parseExpression))
                           , parsePrec3
                           ]
    parsePrec3 = TM.choice [ TM.try (Mul <$> parsePrec2 <*> (token PP.Asterisk *> parseExpression))
                           , parsePrec2
                           ]
    parsePrec2 = TM.choice [ TM.try (Dereference <$> (token PP.Asterisk *> parsePrec1))
                           , TM.try (AddressOf <$> (token PP.Amp *> parsePrec1))
                           , TM.try (Truncate <$> parsePrec1 <*> (token PP.Colon *> parseWord))
                           , parsePrec1
                           ]
    parsePrec1 = TM.choice [ TM.try (Funcall <$> parseIdentifier <*> TM.between (token PP.LParen) (token PP.RParen) (TM.sepBy parseExpression (token PP.Comma)))
                           , TM.try (Ref <$> parseIdentifier)
                           , TM.try (Word_ <$> parseWord)
                           , TM.between (token PP.LParen) (token PP.RParen) parseExpression
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

parseSemantics :: P.SleighM [Stmt]
parseSemantics = TM.many parseStatement
  where
    parseStatement = do
      s <- TM.choice [ TM.try parseExportStmt
                     , TM.try parseAssignStmt
                     , TM.try (ExprStmt <$> parseExpression)
                     ]
      token PP.Semi
      return s
    parseExportStmt = do
      token PP.Export
      v <- TM.choice [ TM.try (ExportedDynamic <$> parseDynamicExport <*> parseIdentifier)
                     , TM.try (ExportedIdentifier <$> parseIdentifier)
                     , ExportedConstant <$> parseWord <*> (token PP.Colon >> parseWord)
                     ]
      return (Export v)
    parseAssignStmt = Assign <$> parseExpression <*> (token PP.Assign *> parseExpression)

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
