module Language.Sleigh.AST (
   Identifier(..)
 , Sleigh(..)
 , Endianness(..)
 , Definition(..)
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

data Definition = DefEndianness !Endianness
                | DefInstructionAliengment !Word
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
      DefInstructionAliengment a -> PP.pretty "Instruction Alignment: " <> PP.pretty a <> PP.pretty " bytes"

instance PP.Pretty Sleigh where
  pretty s = PP.vcat [ PP.vcat (map PP.pretty (F.toList (definitions s)))
                     ]
