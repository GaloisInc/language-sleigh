module Language.Sleigh.Identifier (
  Identifier(..)
  ) where

import qualified Data.Text as DT
import qualified Prettyprinter as PP

newtype Identifier = Identifier { identifierText :: DT.Text }
  deriving (Eq, Ord, Show)

instance PP.Pretty Identifier where
  pretty (Identifier i) = PP.pretty i
