{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
import qualified Prettyprinter as PP
import           System.FilePath ( (</>) )
import qualified Test.Tasty as TT
import qualified Test.Tasty.HUnit as TTH
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh as LS

toPreprocessorTest :: (String, [LS.Token]) -> TT.TestTree
toPreprocessorTest (str, expected) = TTH.testCase str $ do
  let includePath = "/" -- Not used in the tests
  eres <- LS.preprocessSleigh includePath "<test>" (BSC.pack str)
  case eres of
    Left err -> TTH.assertFailure (TM.errorBundlePretty err)
    Right toks -> do
      let actuals = [ LS.tokenVal t
                    | t <- F.toList toks
                    ]
      TTH.assertEqual "Tokens" expected actuals

-- | Manually-written preprocessor tests, verifying that the expected tokens are generated
preprocessorTests :: TT.TestTree
preprocessorTests = TT.testGroup "Preprocessor Tests" (map toPreprocessorTest expected)
  where
    expected = [ ("<<", [LS.ShiftLeft])
               , (">>", [LS.ShiftRight])
               , ("0x21", [LS.Number 0x21])
               , ("& &", [LS.Amp, LS.Amp])
               , ("&&", [LS.LogicalAnd])
               , (":and", [LS.Colon, LS.Ident (LS.Identifier "and")])
               , ("|", [LS.BitwiseOr])
               , ("||", [LS.LogicalOr])
               , ("s<", [LS.SignedLessThan])
               , ("f-", [LS.FSub])
               ]

mipsDir :: FilePath
mipsDir = "ghidra/Ghidra/Processors/MIPS/data/languages"

mipsTargets :: [String]
mipsTargets = [ "mips32le.slaspec"
              , "mips32be.slaspec"
              ]

ghidraPreprocessorTest
  :: FilePath
  -- ^ The include path (and directory containing the target)
  -> String
  -- ^ The target file to preprocess
  -> TT.TestTree
ghidraPreprocessorTest dir target = TTH.testCase ("Preprocess:" ++ name) $ do
  let includePath = dir
  bytes <- BSC.readFile name
  eres <- LS.preprocessSleigh includePath name bytes
  case eres of
    Left err -> TTH.assertFailure (TM.errorBundlePretty err)
    Right _tokens -> return ()
  where
    name = dir </> target

ghidraParserTest
  :: FilePath
  -- ^ The include path (and directory containing the target)
  -> String
  -- ^ The target file to parse
  -> TT.TestTree
ghidraParserTest dir target = TTH.testCase ("Parse:" ++ name) $ do
  let includePath = dir
  bytes <- BSC.readFile name
  eres <- LS.parseSleigh includePath name bytes
  case eres of
    Left err -> TTH.assertFailure (show (PP.pretty err))
    Right _ast -> return ()
  where
    name = dir </> target

allTests :: TT.TestTree
allTests =
  TT.testGroup "All Tests" (concat [ [preprocessorTests]
                                   , fmap (ghidraPreprocessorTest mipsDir) mipsTargets
                                   , fmap (ghidraParserTest mipsDir) mipsTargets
                                   ])

main :: IO ()
main = TT.defaultMain allTests
