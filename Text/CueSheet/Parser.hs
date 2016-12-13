-- |
-- Module      :  Text.CueSheet.Parser
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The modules contains just CUE sheet parser. You probably want to import
-- "Text.CueSheet" instead.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE LambdaCase         #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.CueSheet.Parser
  ( Eec (..)
  , CueParserFailure (..)
  , parseCueSheet )
where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Numeric.Natural
import Text.CueSheet.Types
import Text.Megaparsec
import qualified Data.ByteString.Lazy  as BL
import qualified Data.Set              as E
import qualified Data.Text             as T
import qualified Text.Megaparsec.Lexer as L

----------------------------------------------------------------------------
-- Types

-- | Extended error component with support for storing number of track
-- declaration in which a parsing error has occurred.

data Eec = Eec (Maybe Natural) (Maybe CueParserFailure)
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance ErrorComponent Eec where
  representFail                  =
    Eec Nothing . Just . CueParserFail
  representIndentation ord p0 p1 =
    (Eec Nothing . Just) (CueParserIndentation ord p0 p1)

instance ShowErrorComponent Eec where
  showErrorComponent (Eec mtrack mfailure) =
    maybe "" ((++ "\n") . showErrorComponent) mfailure ++
    maybe "" (\n -> "in declaration of the track " ++ show n) mtrack

-- | Enumeration of all failures that may happen during run of
-- 'parseCueSheet'.

data CueParserFailure
  = CueParserFail String
  | CueParserIndentation Ordering Pos Pos
  | CueParserInvalidCatalog Text
  | CueParserDuplicateCatalog
  | CueParserDuplicateCdTextFile
  | CueParserDuplicatePerformer
  | CueParserDuplicateTitle
  | CueParserDuplicateSongwriter
  | CueParserInvalidCueText Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance ShowErrorComponent CueParserFailure where
  showErrorComponent = \case
    CueParserFail msg ->
      showErrorComponent (DecFail msg)
    CueParserIndentation ord p0 p1 ->
      showErrorComponent (DecIndentation ord p0 p1)
    CueParserInvalidCatalog txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid Media Catalog Number"
    CueParserDuplicateCatalog ->
      "a CUE sheet cannot have two CATALOG declarations"
    CueParserDuplicateCdTextFile ->
      "a CUE sheet cannot have two CDTEXTFILE declarations"
    CueParserDuplicatePerformer ->
      "a CUE sheet cannot have two top-level PERFORMER declarations"
    CueParserDuplicateTitle ->
      "a CUE sheet cannot have two top-level TITLE declarations"
    CueParserDuplicateSongwriter ->
      "a CUE sheet cannot have two top-level SONGWRITER declarations"
    CueParserInvalidCueText txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid CUE text literal"

-- | Type of parser we use here, it's not public.

type Parser a = StateT Context (Parsec Eec BL.ByteString) a

-- | Context of parsing. This is passed around in 'StateT'. We need all of
-- this to signal parse errors on duplicate declaration of things that
-- should only be declared once according to description of the format, to
-- validate track numbers, etc.

data Context = Context
  { contextCueSheet   :: CueSheet
    -- ^ Current state of CUE sheet we parse. When a part\/parameter of CUE
    -- sheet is parsed, this thing is updated.
  , contextFiles      :: [CueFile]
    -- ^ Temporary storage for parsed files (we can't store it in the
    -- 'CueSheet' because it does not allow empty list of files).
  , contextTracks     :: [CueTrack]
    -- ^ Similar to 'contextFiles', collection of tracks but for current
    -- file.
  , contextTrackCount :: Natural
    -- ^ Number of tracks we have parsed so far, to avoid traversing lists
    -- again and again.
  , contextIndexCount :: Natural
    -- ^ Similarly for indices.
  }

-- | Parse a CUE sheet from a lazy 'BL.ByteString'.

parseCueSheet
  :: String            -- ^ File name to include in error messages
  -> BL.ByteString     -- ^ CUE sheet to parsec as a lazy 'BL.ByteString'
  -> Either (ParseError Char Eec) CueSheet -- ^ 'ParseError' or result
parseCueSheet = parse (contextCueSheet <$> execStateT pCueSheet initContext)
  where
    initContext = Context
      { contextCueSheet = CueSheet
        { cueCatalog          = Nothing
        , cueCdTextFile       = Nothing
        , cuePerformer        = Nothing
        , cueTitle            = Nothing
        , cueSongwriter       = Nothing
        , cueFirstTrackNumber = 0
        , cueFiles            = dummyFile :| [] }
      , contextFiles          = []
      , contextTracks         = []
      , contextTrackCount     = 0
      , contextIndexCount     = 0 }

-- | Parse a 'CueSheet'. The result is not returned, but written in
-- 'Context'.

pCueSheet :: Parser ()
pCueSheet =
  void (many pHeaderItem)
  -- TODO FILE declarations and everything inside

pHeaderItem :: Parser ()
pHeaderItem = choice
  [ pCatalog
  , pCdTextFile
  , pPerformer
  , pTitle
  , pSongwriter ]

pCatalog :: Parser ()
pCatalog = do
  already <- gets (isJust . cueCatalog . contextCueSheet)
  let f x =
        if already
          then Left CueParserDuplicateCatalog
          else case mkMcn x of
                 Nothing -> Left (CueParserInvalidCatalog x)
                 Just mcn -> Right mcn
  mcn <- withCheck f (T.pack <$> labelledLit "CATALOG")
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueCatalog = Just mcn } }

pCdTextFile :: Parser ()
pCdTextFile = do
  already <- gets (isJust . cueCdTextFile . contextCueSheet)
  let f x =
        if already
          then Left CueParserDuplicateCdTextFile
          else Right x
  cdTextFile <- withCheck f (labelledLit "CDTEXTFILE")
  modify $ \x -> x { contextCueSheet = (contextCueSheet x)
    { cueCdTextFile = Just cdTextFile } }

pPerformer :: Parser ()
pPerformer = do
  already <- gets (isJust . cuePerformer . contextCueSheet)
  let f x =
        if already
          then Left CueParserDuplicatePerformer
          else case mkCueText x of
                 Nothing -> Left (CueParserInvalidCueText x)
                 Just txt -> Right txt
  performer <- withCheck f (T.pack <$> labelledLit "PERFORMER")
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cuePerformer = Just performer } }

pTitle :: Parser ()
pTitle = do
  already <- gets (isJust . cueTitle . contextCueSheet)
  let f x =
        if already
          then Left CueParserDuplicateTitle
          else case mkCueText x of
                 Nothing -> Left (CueParserInvalidCueText x)
                 Just txt -> Right txt
  title <- withCheck f (T.pack <$> labelledLit "TITLE")
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueTitle = Just title } }

pSongwriter :: Parser ()
pSongwriter = do
  already <- gets (isJust . cueTitle . contextCueSheet)
  let f x =
        if already
          then Left CueParserDuplicateSongwriter
          else case mkCueText x of
                 Nothing -> Left (CueParserInvalidCueText x)
                 Just txt -> Right txt
  songwriter <- withCheck f (T.pack <$> labelledLit "SONGWRITER")
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueSongwriter = Just songwriter } }

labelledLit :: String -> Parser String
labelledLit command = do
  void (symbol command)
  r <- lexeme stringLit
  void eol
  return r

----------------------------------------------------------------------------
-- Helpers

-- | Parse a thing and then check if it's OK conceptually. If it's not OK,
-- the error will be reported with position at the start of offending
-- lexeme, otherwise the lexeme is parsed as usual. Of course if the lexeme
-- has incorrect format, that is just reported and no additional check
-- happens.

withCheck :: (a -> Either CueParserFailure b) -> Parser a -> Parser b
withCheck check p = do
  r <- lookAhead p
  case check r of
    Left custom -> failure E.empty E.empty $
      E.singleton (Eec Nothing (Just custom))
    Right x -> x <$ p

-- | Indicate that the inner parser belongs to declaration of track with
-- given index. The index of the track will be added to 'ParseError's to
-- help user find where the error happened.

inTrack :: Natural -> Parser a -> Parser a
inTrack n m = do
  r <- observing m
  case r of
    Left ParseError {..} ->
      failure errorUnexpected errorExpected $
        if E.null errorCustom
          then E.singleton (Eec (Just n) Nothing)
          else E.map f errorCustom
        where
          f (Eec mn x) = Eec (mn <|> Just n) x
    Right x -> return x

-- | String literal with support for quotation.

stringLit :: Parser String
stringLit = quoted <|> unquoted
  where
    quoted   = char '\"' *> manyTill (noneOf "\n") (char '\"')
    unquoted = many (noneOf "\n\t ")

-- | Case-insensitive symbol parser.

symbol :: String -> Parser String
symbol s = L.symbol' sc s <* notFollowedBy alphaNumChar

-- | A wrapper for lexemes.

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Space consumer (does not eat newlines).

sc :: Parser ()
sc = L.space (void $ oneOf "\t ") empty empty

----------------------------------------------------------------------------
-- Dummies

-- | A dummy file. It's only here because 'CueSheet' can't have an empty
-- list of files and it cannot be a bottom either.

dummyFile :: CueFile
dummyFile = CueFile
  { cueFileName   = ""
  , cueFileType   = Wave
  , cueFileTracks = dummyTrack :| [] }

-- | A dummy track, see 'dummyFile'.

dummyTrack :: CueTrack
dummyTrack = CueTrack
  { cueTrackDigitalCopyPermitted = False
  , cueTrackFourChannelAudio     = False
  , cueTrackPreemphasisEnabled   = False
  , cueTrackSerialCopyManagement = False
  , cueTrackType                 = CueTrackAudio
  , cueTrackIsrc                 = Nothing
  , cueTrackTitle                = Nothing
  , cueTrackPerformer            = Nothing
  , cueTrackSongwriter           = Nothing
  , cueTrackPregap               = Nothing
  , cueTrackPregapIndex          = Nothing
  , cueTrackIndices              = CueTime 0 :| []
  , cueTrackPostgap              = Nothing }
