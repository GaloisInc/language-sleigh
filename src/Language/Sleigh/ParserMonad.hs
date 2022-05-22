{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
module Language.Sleigh.ParserMonad (
    SleighParseError(..)
  , TokenStream
  , SleighM
  , runSleigh
  -- * State modifiers
  , recordDefinition
  , recordAttach
  -- * State inspection
  , definitions
  , attachments
  ) where

import           Control.Applicative ( Alternative )
import qualified Control.Lens as CL
import qualified Control.Lens.TH as CLT
import           Control.Monad ( MonadPlus )
import qualified Control.Monad.RWS.Strict as CMR
import qualified Data.Foldable as F
import qualified Data.List as DL
import qualified Data.List.NonEmpty as DLN
import           Data.Proxy ( Proxy(..) )
import qualified Data.Sequence as Seq
import qualified Data.Text as DT
import qualified Prettyprinter as PP
import qualified Text.Megaparsec as TM

import qualified Language.Sleigh.AST as A
import qualified Language.Sleigh.Preprocessor as P

-- | State required to parse sleigh files
--
-- This includes preprocessor state and any extra information required from includes
data ParserState =
  ParserState { _definedStmts :: Seq.Seq A.Definition
              -- ^ Language-level define statements
              , _attachStmts :: Seq.Seq A.Attach
              -- ^ Modifiers attaching meaning to definitions
              }

$(CLT.makeLenses ''ParserState)

-- | Constant environment data for the parser
data ParserEnv =
  ParserEnv {
            }

-- | Parser errors from sleigh semantics (rather than pure syntax errors, which are reported by megaparsec)
data SleighParseError = InvalidEndianness !DT.Text
                      | SleighPreprocessingError !P.SleighPreprocessingError
  deriving (Eq, Ord, Show)

instance TM.ShowErrorComponent SleighParseError where
  showErrorComponent e =
    case e of
      InvalidEndianness t -> "InvalidEndianness: " <> DT.unpack t
      SleighPreprocessingError ppErr -> TM.showErrorComponent ppErr

data TokenStream =
  TokenStream { streamInput :: String
              , unTokenStream :: [P.Positioned P.Token]
              }

instance TM.Stream TokenStream where
  type Token TokenStream = P.Positioned P.Token
  type Tokens TokenStream = [P.Positioned P.Token]
  tokenToChunk _ x = [x]
  tokensToChunk _ xs = xs
  chunkToTokens _ = id
  chunkLength _ = length
  chunkEmpty _ = null
  take1_ (TokenStream _ []) = Nothing
  take1_ (TokenStream str (t:ts)) =
    Just (t, TokenStream (drop (TM.tokensLength (Proxy @TokenStream) (t DLN.:| [])) str) ts)
  takeN_ n ts0@(TokenStream str tks)
    | n <= 0 = Just ([], ts0)
    | null tks = Nothing
    | otherwise =
      let (x, tks') = splitAt n tks
      in case DLN.nonEmpty x of
        Nothing -> Just (x, TokenStream str tks')
        Just nex -> Just (x, TokenStream (drop (TM.tokensLength (Proxy @TokenStream) nex) str) tks')
  takeWhile_ f (TokenStream str tks) =
    let (x, tks') = DL.span f tks
    in case DLN.nonEmpty x of
      Nothing -> (x, TokenStream str tks')
      Just nex -> (x, TokenStream (drop (TM.tokensLength (Proxy @TokenStream) nex) str) tks')

instance TM.VisualStream TokenStream where
  showTokens Proxy = DL.intercalate " " . DLN.toList . fmap (show . PP.pretty . P.tokenVal)
  tokensLength Proxy xs = sum ((fromIntegral . P.tokenLength) <$> xs)

instance TM.TraversableStream TokenStream where
  reachOffset o posState =
    let (pre, post) = splitAt (o - TM.pstateOffset posState) (unTokenStream (TM.pstateInput posState))
        newSourcePos =
          case post of
            [] -> TM.pstateSourcePos posState
            (x:_) -> P.startPos x
        sameLine = TM.sourceLine newSourcePos == TM.sourceLine (TM.pstateSourcePos posState)
        tokensConsumed =
          case DLN.nonEmpty pre of
            Nothing -> 0
            Just nePre -> TM.tokensLength (Proxy @TokenStream) nePre
        (preStr, postStr) = splitAt tokensConsumed (streamInput (TM.pstateInput posState))
        preLine = reverse $ takeWhile (/= '\n') $ reverse preStr
        prefix
          | sameLine = TM.pstateLinePrefix posState <> preLine
          | otherwise = preLine
        restOfLine = takeWhile (/= '\n') postStr
        newPos = TM.PosState { TM.pstateInput = TokenStream postStr post
                             , TM.pstateOffset = max (TM.pstateOffset posState) o
                             , TM.pstateSourcePos = newSourcePos
                             , TM.pstateTabWidth = TM.pstateTabWidth posState
                             , TM.pstateLinePrefix = prefix
                             }
    in (Just (prefix <> restOfLine), newPos)

-- | A monad for parsing Sleigh that includes the necessary state
--
-- Note that while this is a transformer over IO, we don't export the full power
-- of IO. We only expose the primitives necessary to handle include files and
-- other restricted operations.
newtype SleighM a = SleighM { unSleigh :: TM.ParsecT SleighParseError TokenStream (CMR.RWST ParserEnv () ParserState IO) a }
  deriving ( Functor
           , Applicative
           , Alternative
           , Monad
           , MonadPlus
           , TM.MonadParsec SleighParseError TokenStream
           )

recordDefinition
  :: A.Definition
  -> SleighM ()
recordDefinition d =
  SleighM $ definedStmts CL.%= (Seq.|> d)

recordAttach
  :: A.Attach
  -> SleighM ()
recordAttach a =
  SleighM $ attachStmts CL.%= (Seq.|> a)

definitions :: SleighM (Seq.Seq A.Definition)
definitions = SleighM (CL.use definedStmts)

attachments :: SleighM (Seq.Seq A.Attach)
attachments = SleighM (CL.use attachStmts)

-- | Run a 'SleighM' parser
--
-- Note that this must be in 'IO' because it resolves includes dynamically
runSleigh
  :: (F.Foldable f)
  => FilePath
  -- ^ The input filename
  -> DT.Text
  -- ^ The original input string (for error messages)
  -> f (P.Positioned P.Token)
  -- ^ The preprocessed token stream
  -> SleighM a
  -- ^ The parser to run
  -> IO (Either (TM.ParseErrorBundle TokenStream SleighParseError) a)
runSleigh filename str0 tokens p =
  fst <$> CMR.evalRWST (TM.runParserT (unSleigh p) filename stream) env s0
  where
    env = ParserEnv {
                    }
    s0 = ParserState { _definedStmts = mempty
                     , _attachStmts = mempty
                     }
    stream = TokenStream { unTokenStream = F.toList tokens
                         , streamInput = DT.unpack str0
                         }
