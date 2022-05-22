module Language.Sleigh.AST (
   Identifier(..)
 , Sleigh(..)
 , Endianness(..)
 , Definition(..)
 , Attach(..)
 , ValueInterpretation(..)
 , Default(..)
 , SpaceType(..)
 , Attribute(..)
 , ContextAttribute(..)
 , ContextField(..)
 , TokenField(..)
 ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
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

-- | Statements that define the base constructs of an Sleigh spec
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

-- | Interpretations for attached variables
data ValueInterpretation = ValidInterpretation !Identifier
                         -- ^ Interpret the value as the given identifier
                         | InvalidInterpretation
                         -- ^ The value is an invalid decoding
                         deriving (Show)

-- | Statements that attach additional meaning to fields
data Attach = AttachVariables (DLN.NonEmpty Identifier) [ValueInterpretation]
              -- ^ @fieldlist, registerlist@
              --
              -- Assigns meanings to each bit-pattern of the named fields. For
              -- example, if a field @F@ is two bits, it has 4 possible integer
              -- values. The integer values of @F@ become indexes into the
              -- registerlist, giving interpretations for each bit pattern.  The
              -- registerlist can contain underscores to denote invalid encodings.
              --
              -- From the Ghidra docs:
              --
              -- @
              --    Probably the most common processor interpretation of a field is
              --    as an encoding of a particular register. In SLEIGH this can be
              --    done with the attach variables statement:
              --
              --     attach variables fieldlist registerlist;
              --
              --    A fieldlist can be a single field identifier or a space
              --    separated list of field identifiers surrounded by square
              --    brackets. A registerlist must be a square bracket surrounded
              --    and space separated list of register identifiers as created
              --    with define statements (see Section Section 4.4, “Naming
              --    Registers”). For each field in the fieldlist, instead of
              --    having the display and semantic meaning of an integer, the
              --    field becomes a look-up table for the given list of
              --    registers. The original integer interpretation is used as
              --    the index into the list starting at zero, so a specific
              --    instruction that has all the bits in the field equal to zero
              --    yields the first register (a specific varnode) from the list
              --    as the meaning of the field in the context of that
              --    instruction. Note that both the display and semantic meaning
              --    of the field are now taken from the new register.
              --
              --    A particular integer can remain unspecified by putting a ‘_’
              --    character in the appropriate position of the register list or
              --    also if the length of the register list is less than the
              --    integer. A specific integer encoding of the field that is
              --    unspecified like this does not revert to the original semantic
              --    and display meaning. Instead this encoding is flagged as an
              --    invalid form of the instruction.
              -- @
  deriving (Show)

data Sleigh =
  Sleigh { definitions :: Seq.Seq Definition
         , attachments :: Seq.Seq Attach
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
