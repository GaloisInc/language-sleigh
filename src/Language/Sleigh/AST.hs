module Language.Sleigh.AST (
   Identifier(..)
 , Sleigh(..)
 , Endianness(..)
 , Definition(..)
 , Default(..)
 , SpaceType(..)
 , Attribute(..)
 , ContextAttribute(..)
 , ContextField(..)
 , TokenField(..)
 ) where

import qualified Data.Foldable as F
import qualified Data.Sequence as Seq
import qualified Data.Text as DT
import qualified Prettyprinter as PP

newtype Identifier = Identifier { identifierText :: DT.Text }
  deriving (Eq, Ord, Show)

instance PP.Pretty Identifier where
  pretty (Identifier i) = PP.pretty i

data Endianness = Little | Big
  deriving (Show)

data Default = IsDefault | NotDefault
  deriving (Show)

data SpaceType = Memory | Register
  deriving (Show)

data Attribute = Signed | Decimal | Hexadecimal
  deriving (Show)

data ContextAttribute = NormalAttribute Attribute | NoFlow
  deriving (Show)

data ContextField =
  ContextField { contextFieldName :: !Identifier
               , contextFieldLo :: !Word
               , contextFieldHi :: !Word
               , contextAttributes :: [ContextAttribute]
               }
  deriving (Show)

data TokenField =
  TokenField { tokenFieldName :: !Identifier
             , tokenFieldLo :: !Word
             , tokenFieldHi :: !Word
             , tokenAttributes :: [Attribute]
             }
  deriving (Show)

data Definition = DefEndianness !Endianness
                | DefInstructionAlignment !Word
                -- ^ Alignment in bytes
                | DefSpace !Identifier !SpaceType !Word !Default
                -- ^ Name of space, type of space, address size, Default
                | DefRegisterBank !Word !Word [Identifier]
                -- ^ Offset, Register size
                | DefContextVariables !Identifier [ContextField]
                -- ^ Name, fields
                | DefTokenFields !Identifier !Word [TokenField]
                -- ^ Name, number of bits, fields
  deriving (Show)

data Sleigh =
  Sleigh { definitions :: Seq.Seq Definition
         }
  deriving (Show)


instance PP.Pretty Endianness where
  pretty e =
    case e of
      Big -> PP.pretty "big"
      Little -> PP.pretty "little"

instance PP.Pretty Definition where
  pretty d =
    case d of
      DefEndianness e -> PP.pretty "Endianness: " <> PP.pretty e
      DefInstructionAlignment a -> PP.pretty "Instruction Alignment: " <> PP.pretty a <> PP.pretty " bytes"

instance PP.Pretty Sleigh where
  pretty s = PP.vcat [ PP.vcat (map PP.pretty (F.toList (definitions s)))
                     ]
