{-# LANGUAGE OverloadedStrings #-}
module Language.Sleigh.Token (
    Token(..)
  , Positioned(..)
  ) where

import qualified Data.Text as DT
import qualified Prettyprinter as PP
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh.Identifier as I

data Token = StringLiteral DT.Text
           | Number Int
           | Ident I.Identifier
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
           | Asterisk
           | Plus
           | Minus
           | ShiftLeft
           | ShiftRight
           | Dollar
           | LogicalOr
           | LogicalAnd
           | BitwiseOr
           | Equals
           | NotEquals
           | SignedGreaterEquals
           | GreaterEquals
           | SignedGreaterThan
           | GreaterThan
           | SignedLessEquals
           | LessEquals
           | SignedLessThan
           | LessThan
           | BitwiseNot
           | Caret
           | Exclamation
           | Div
           | SDiv
           | Mod
           | SMod
           | SignedShiftRight
           | FAdd
           | FDiv
           | FMul
           | FSub
           -- Special identifiers
           | Define
           | Attach
           | Variables
           | Export
           | Macro
           | Is
           deriving (Eq, Ord, Show)

instance PP.Pretty Token where
  pretty t =
    case t of
      StringLiteral s -> PP.pretty '"' <> PP.viaShow s <> PP.pretty '"'
      Number i -> PP.pretty i
      Ident i -> "Ident" <> PP.parens (PP.pretty i)
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
      Asterisk -> "*"
      Plus -> "+"
      Div -> "/"
      SDiv -> "s/"
      Mod -> "%"
      SMod -> "s%"
      Minus -> "-"
      Equals -> "=="
      NotEquals -> "!="
      ShiftLeft -> "<<"
      ShiftRight -> ">>"
      SignedShiftRight -> "s>>"
      Dollar -> "$"
      LogicalOr -> "||"
      LogicalAnd -> "&&"
      BitwiseOr -> "|"
      FAdd -> "f+"
      FSub -> "f-"
      FDiv -> "f/"
      FMul -> "f*"
      Caret -> "^"
      Exclamation -> "!"
      SignedGreaterEquals -> "s>="
      GreaterEquals -> ">="
      SignedGreaterThan -> "s>"
      GreaterThan -> ">"
      SignedLessEquals -> "s<="
      LessEquals -> "<="
      SignedLessThan -> "s<"
      LessThan -> "<"
      BitwiseNot -> "~"
      Define -> "define"
      Attach -> "attach"
      Variables -> "variables"
      Export -> "export"
      Macro -> "macro"
      Is -> "is"


data Positioned a =
  WithPos { startPos :: TM.SourcePos
          , endPos :: TM.SourcePos
          , tokenLength :: Word
          , tokenVal :: a
          }
  deriving (Eq, Ord, Show)

instance (PP.Pretty a) => PP.Pretty (Positioned a) where
  pretty p = PP.hcat [ PP.pretty (tokenVal p)
                     , "@"
                     , PP.pretty (TM.sourcePosPretty (startPos p))
                     , "-"
                     , PP.pretty (TM.sourcePosPretty (endPos p))
                     ]
