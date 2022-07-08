{-# LANGUAGE OverloadedStrings #-}
module Main ( main ) where

import qualified Data.ByteString.Char8 as BSC
import qualified Data.Foldable as F
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

preprocessorTests :: TT.TestTree
preprocessorTests = TT.testGroup "Preprocessor Tests" (map toPreprocessorTest expected)
  where
    expected = [ ("<<", [LS.ShiftLeft])
               , (">>", [LS.ShiftRight])
               , ("0x21", [LS.Number 0x21])
               , ("& &", [LS.Amp, LS.Amp])
               , ("&&", [LS.LogicalAnd])
               , (":and", [LS.Colon, LS.Ident (LS.Identifier "and")])
               ]

allTests :: TT.TestTree
allTests =
  TT.testGroup "All Tests" [ preprocessorTests ]

main :: IO ()
main = TT.defaultMain allTests
