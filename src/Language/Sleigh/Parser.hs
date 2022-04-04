{-# LANGUAGE OverloadedStrings #-}
module Language.Sleigh.Parser (
  sleighParser
  ) where

import           Control.Applicative ( empty, (<|>) )
import qualified Data.Text as DT
import           Text.Megaparsec ( (<?>) )
import qualified Text.Megaparsec as TM
import qualified Text.Megaparsec.Char as TMC
import qualified Text.Megaparsec.Char.Lexer as TMCL

import           Language.Sleigh.AST
import qualified Language.Sleigh.ParserMonad as P

spaceConsumer :: P.SleighM ()
spaceConsumer = TMCL.space TMC.space1 lineComment blockComment
  where
    lineComment = TMCL.skipLineComment "#"
    blockComment = empty

lexeme :: P.SleighM a -> P.SleighM a
lexeme = TMCL.lexeme spaceConsumer

-- | Parse an identifier
identifier :: P.SleighM Identifier
identifier = do
  c1 <- identSymbols <|> TMC.letterChar
  cs <- TM.many (TMC.alphaNumChar <|> identSymbols)
  return (Identifier (DT.pack (c1 : cs)))
  where
    identSymbols = TM.satisfy (\c -> c == '_' || c == '.')

-- | Parse a string literal
--
-- Note that there does not seem to be an escape syntax according to the Sleigh documentation
--
-- Does not include the double quotes
stringLiteral :: P.SleighM DT.Text
stringLiteral = do
  _ <- TMC.char '"'
  s <- TM.many (TM.anySingleBut '"')
  _ <- TMC.char '"'
  return (DT.pack s)

parseComment :: P.SleighM ()
parseComment = do
  _ <- TMC.char '#'
  _ <- TM.manyTill TM.anySingle TMC.eol
  return ()

-- | @\@define IDENT val@
parsePreprocessorDefine :: P.SleighM ()
parsePreprocessorDefine = do
  _ <- lexeme (TMC.string "define")
  ident <- lexeme identifier <?> "Preprocessor definition identifier"
  -- The definition can be either a bare identifier or a quoted literal; we have
  -- to unwrap the type wrapper from identifiers
  value <- lexeme (TM.try stringLiteral <|> (identifierText <$> identifier)) <?> "Preprocessor definition value"
  P.recordPreprocessorDefinition ident value

-- | @\@undef IDENT@
parsePreprocessorUndefine :: P.SleighM ()
parsePreprocessorUndefine = do
  _ <- lexeme (TMC.string "undef")
  ident <- lexeme identifier
  P.undefinePreprocessor ident

parsePreprocessorInclude :: P.SleighM ()
parsePreprocessorInclude = do
  _ <- lexeme (TMC.string "include")
  filename <- lexeme stringLiteral
  P.processInclude filename (TM.some topLevel >> TM.eof)

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
parsePreprocessorDirective :: P.SleighM ()
parsePreprocessorDirective = do
  TM.choice [ TM.try parsePreprocessorDefine
            , TM.try parsePreprocessorUndefine
            -- Note that this needs to come last so that errors in included
            -- files can be propagated properly; also it must not be wrapped in
            -- a 'TM.try'
            , parsePreprocessorInclude
            ]

-- | Parse a top-level entity
--
-- Each top-level entity is recorded and/or processed as a mutation of the parser state
topLevel :: P.SleighM ()
topLevel =
  TM.choice [ TM.try parseComment
            , TM.try (TMC.eol >> return ())
            -- Note that this needs to come last so that errors in included
            -- files can be propagated properly; also it must not be wrapped in
            -- a 'TM.try'
            , (TMC.char '@' *> parsePreprocessorDirective)
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
