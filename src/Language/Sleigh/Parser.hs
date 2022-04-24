{-# LANGUAGE GADTs #-}
{-# LANGUAGE OverloadedStrings #-}
module Language.Sleigh.Parser (
  sleighParser
  ) where

import           Control.Applicative ( (<|>) )
import qualified Data.Text as DT
import qualified Data.Text.Read as DTR
import           Text.Megaparsec ( (<?>) )
import qualified Text.Megaparsec as TM

import           Language.Sleigh.AST
import qualified Language.Sleigh.ParserMonad as P
import qualified Language.Sleigh.Preprocessor as PP

import Debug.Trace
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
        PP.Identifier i -> identifierText i == DT.pack t
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
  return Sleigh { definitions = defs
                }
