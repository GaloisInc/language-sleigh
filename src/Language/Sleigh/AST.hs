module Language.Sleigh.AST (
   Sleigh(..)
 , Endianness(..)
 , Definition(..)
 , Attach(..)
 , ValueInterpretation(..)
 , Constructor(..)
 , Macro(..)
 , Stmt(..)
 , Expr(..)
 , DynamicExport(..)
 , ExportedValue(..)
 , TableHeader(..)
 , BitPattern(..)
 , Constraint(..)
 , Default(..)
 , SpaceType(..)
 , Attribute(..)
 , ContextAttribute(..)
 , ContextField(..)
 , TokenField(..)
 , JumpTarget(..)
 ) where

import qualified Data.Foldable as F
import qualified Data.List.NonEmpty as DLN
import qualified Data.Sequence as Seq
import qualified Data.Text as DT
import qualified Prettyprinter as PP

import           Language.Sleigh.Identifier
import qualified Language.Sleigh.Token as T

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
                | DefPCodeOp !Identifier
                -- ^ The name of a custom pcode operation
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
              | AttachNames (DLN.NonEmpty Identifier) [DT.Text]
              -- ^ Attach additional names to fields, but do not change their semantic meaning
              --
              -- @
              --
              -- It is possible to just modify the display characteristics of
              --  a field without changing the semantic meaning. The need for
              --  this is rare, but it is possible to treat a field as having
              --  influence on the display of the disassembly but having no
              --  influence on the semantics. Even if the bits of the field do
              --  have some semantic meaning, sometimes it is appropriate to
              --  define overlapping fields, one of which is defined to have
              --  no semantic meaning. The most convenient way to break down
              --  the required disassembly may not be the most convenient way
              --  to break down the semantics. It is also possible to have
              --  symbols with semantic meaning but no display meaning (see
              --  Section 7.4.5, “Invisible Operands”).
              --
              --  At any rate we can list the display interpretation of a
              --  field directly with an attach names statement.
              --
              --    attach names fieldlist stringlist;
              --
              --  The stringlist is assigned to each of the fields in the same
              --  manner as the attach variables and attach values statements. A
              --  specific encoding of the field now displays as the string in
              --  the list at that integer position. Field values greater than
              --  the size of the list are interpreted as invalid encodings.
              --
              -- @

  deriving (Show)

-- | Constraints in bit patterns
data Constraint = EqualityConstraint !Identifier !Word
                -- ^ field name, value
                | Unconstrained !Identifier
                | StringConstraint DT.Text
                deriving (Show)

data BitPattern = And BitPattern BitPattern
                | Or BitPattern BitPattern
                | Constraint !Constraint
                deriving (Show)

data Expr = Ref !Identifier
          | Dereference !Expr
          | AddressOf !Expr
          | Word_ !Word
          | VarNodeRef !Identifier
          -- Bits
          | Truncate !Expr !Word
          -- ^ Integer expression, number of bytes to truncate to
          | ShiftLeft !Expr !Expr
          | ShiftRight !Expr !Expr
          -- Arithmetic
          | Add !Expr !Expr
          | Sub !Expr !Expr
          | Mul !Expr !Expr
          | BitwiseOr !Expr !Expr
          | BitwiseAnd !Expr !Expr
          | BitNot !Expr
          | Funcall !Identifier [Expr]
          | Negate !Expr
          -- ^ Unary arithmetic negation

          -- Relational comparisons
          | RelEquals !Expr !Expr
          | RelNotEquals !Expr !Expr
          | RelLT !Expr !Expr
          | RelLE !Expr !Expr
          | RelGT !Expr !Expr
          | RelGE !Expr !Expr
          | RelSLT !Expr !Expr
          | RelSLE !Expr !Expr
          | RelSGT !Expr !Expr
          | RelSGE !Expr !Expr
  deriving (Show)

data DynamicExport =
  DynamicExport { dynamicAddressSpace :: Maybe Identifier
                , dynamicSize :: !Int
                }
  deriving (Show)

data ExportedValue = ExportedIdentifier !Identifier
                   | ExportedConstant !Word !Word
                   -- ^ Constant, varnode size in bytes
                   | ExportedDynamic !DynamicExport !Identifier
                   deriving (Show)

data JumpTarget = IdentifierTarget !Identifier
                | VarNodeTarget !Identifier
                deriving (Show)

data Stmt = Export !ExportedValue
          | Assign !Expr !Expr
          -- ^ LHS, RHS
          | ExprStmt !Expr
          -- ^ A bare expression (likely a macro expansion or an arch-specific uninterpreted function)
          | Goto !JumpTarget
          -- ^ A control flow transfer to the address held in the given variable
          --
          -- Note the target can be an identifier or a varnode; this could be a separate jump target type
          | If !Expr [Stmt] [Stmt]
          -- ^ If-then-else statements
          | Call !JumpTarget
          -- ^ Call statements
          | Return !JumpTarget
          -- ^ Return statements
          | Local !Identifier !Expr
          -- ^ Local definitions
          | Build !Identifier
          -- ^ Inline PCode in a specific location
  deriving (Show)

data TableHeader = Root
                 | Table Identifier
                 deriving  (Show)

data Constructor =
  Constructor { tableHeader :: TableHeader
              , displaySection :: [T.Positioned T.Token]
              , bitPatterns :: BitPattern
              , disassemblyActions :: [Stmt]
              , constructorStatements :: [Stmt]
              }
  deriving (Show)

data Macro =
  Macro { macroName :: !Identifier
        , macroArguments :: [Identifier]
        , macroStatements :: [Stmt]
        }
  deriving (Show)

data Sleigh =
  Sleigh { definitions :: Seq.Seq Definition
         , attachments :: Seq.Seq Attach
         , constructors :: Seq.Seq Constructor
         , macros :: Seq.Seq Macro
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
