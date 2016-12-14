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
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}

module Text.CueSheet.Parser
  ( Eec (..)
  , CueParserFailure (..)
  , parseCueSheet )
where

import Control.Applicative
import Control.Monad.State.Strict
import Data.Bool (bool)
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
import qualified Data.List.NonEmpty    as NE
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
  | CueParserInvalidCueText Text
  | CueParserDuplicateCatalog
  | CueParserDuplicateCdTextFile
  | CueParserDuplicatePerformer
  | CueParserDuplicateTitle
  | CueParserDuplicateSongwriter
  | CueParserTrackOutOfOrder
  | CueParserDuplicateTrackFlags
  | CueParserDuplicateTrackIsrc
  | CueParserInvalidTrackIsrc Text
  | CueParserDuplicateTrackPerformer
  | CueParserDuplicateTrackTitle
  | CueParserDuplicateTrackSongwriter
  | CueParserDuplicateTrackPregap
  | CueParserDuplicateTrackPostgap
  | CueParserInvalidTime Text
  deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance ShowErrorComponent CueParserFailure where
  showErrorComponent = \case
    CueParserFail msg ->
      showErrorComponent (DecFail msg)
    CueParserIndentation ord p0 p1 ->
      showErrorComponent (DecIndentation ord p0 p1)
    CueParserInvalidCatalog txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid Media Catalog Number"
    CueParserInvalidCueText txt ->
      "the value \"" ++ T.unpack txt ++ "\" is not a valid CUE text literal"
    CueParserDuplicateCatalog ->
      "a CUE sheet can have only one CATALOG declaration"
    CueParserDuplicateCdTextFile ->
      "a CUE sheet can have only one CDTEXTFILE declaration"
    CueParserDuplicatePerformer ->
      "a CUE sheet can have only one top-level PERFORMER declaration"
    CueParserDuplicateTitle ->
      "a CUE sheet can have only one top-level TITLE declaration"
    CueParserDuplicateSongwriter ->
      "a CUE sheet can have only one top-level SONGWRITER declaration"
    CueParserTrackOutOfOrder ->
      "this track appears out of order"
    CueParserDuplicateTrackFlags ->
      "a track can have only one FLAGS declaration"
    CueParserDuplicateTrackIsrc ->
      "a track can have only one ISRC declaration"
    CueParserInvalidTrackIsrc txt ->
      "\"" ++ T.unpack txt ++ "\" is not a valid ISRC"
    CueParserDuplicateTrackPerformer ->
      "a track can have only one PERFORMER declaration"
    CueParserDuplicateTrackTitle ->
      "a track can have only one TITLE declaration"
    CueParserDuplicateTrackSongwriter ->
      "a track can have only one SONGWRITER declaration"
    CueParserDuplicateTrackPregap ->
      "a track can have only one PREGAP declaration"
    CueParserDuplicateTrackPostgap ->
      "a track can have only one POSTGAP declaration"
    CueParserInvalidTime txt ->
      "\"" ++ T.unpack txt ++ "\" is not a valid duration or time position"

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
pCueSheet = do
  scn
  void (many pHeaderItem)
  void (some pFile)
  -- NOTE Lens would help here, but let's keep this vanilla.
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x)
      { cueFiles = (NE.fromList . reverse . contextFiles) x } }
  eof

pHeaderItem :: Parser ()
pHeaderItem = choice
  [ pCatalog
  , pCdTextFile
  , pPerformer
  , pTitle
  , pSongwriter
  , pRem ]

pCatalog :: Parser ()
pCatalog = do
  already <- bool Nothing (Just CueParserDuplicateCatalog)
    <$> gets (isJust . cueCatalog . contextCueSheet)
  let f x' = let x = T.pack x' in
        case mkMcn x of
          Nothing -> Left (CueParserInvalidCatalog x)
          Just mcn -> Right mcn
  mcn <- labelledLit already f "CATALOG"
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueCatalog = Just mcn } }

pCdTextFile :: Parser ()
pCdTextFile = do
  already <- bool Nothing (Just CueParserDuplicateCdTextFile)
    <$> gets (isJust . cueCdTextFile . contextCueSheet)
  cdTextFile <- labelledLit already Right "CDTEXTFILE"
  modify $ \x -> x { contextCueSheet = (contextCueSheet x)
    { cueCdTextFile = Just cdTextFile } }

pPerformer :: Parser ()
pPerformer = do
  already <- bool Nothing (Just CueParserDuplicatePerformer)
    <$> gets (isJust . cuePerformer . contextCueSheet)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  performer <- labelledLit already f "PERFORMER"
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cuePerformer = Just performer } }

pTitle :: Parser ()
pTitle = do
  already <- bool Nothing (Just CueParserDuplicateTitle)
    <$> gets (isJust . cueTitle . contextCueSheet)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  title <- labelledLit already f "TITLE"
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueTitle = Just title } }

pSongwriter :: Parser ()
pSongwriter = do
  already <- bool Nothing (Just CueParserDuplicateSongwriter)
    <$> gets (isJust . cueTitle . contextCueSheet)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  songwriter <- labelledLit already f "SONGWRITER"
  modify $ \x -> x { contextCueSheet =
    (contextCueSheet x) { cueSongwriter = Just songwriter } }

pRem :: Parser ()
pRem = do
  void (symbol "REM")
  manyTill anyChar eol *> scn

pFile :: Parser ()
pFile = do
  void (symbol "FILE")
  filename <- lexeme stringLit
  let pFiletype = choice
        [ Binary   <$ symbol "BINARY"
        , Motorola <$ symbol "MOTOROLA"
        , Aiff     <$ symbol "AIFF"
        , Wave     <$ symbol "WAVE"
        , MP3      <$ symbol "MP3" ]
  filetype <- pFiletype <* eol <* scn
  void (some pTrack)
  tracks <- gets contextTracks
  let newFile = CueFile
        { cueFileName   = filename
        , cueFileType   = filetype
        , cueFileTracks = NE.fromList tracks }
  modify $ \x -> x
    { contextFiles  = newFile : contextFiles x
    , contextTracks = [] }

pTrack :: Parser ()
pTrack = do
  void (symbol "TRACK")
  firstTrack  <- gets (null . contextTracks)
  trackOffset <- gets (cueFirstTrackNumber . contextCueSheet)
  trackCount  <- gets contextTrackCount
  let f x =
        if firstTrack || x == trackOffset + trackCount + 1
          then Right x
          else Left CueParserTrackOutOfOrder
  n <- withCheck f (fromIntegral <$> lexeme L.integer)
  let pTrackType = choice
        [ CueTrackAudio      <$ symbol "AUDIO"
        , CueTrackCdg        <$ symbol "CDG"
        , CueTrackMode1_2048 <$ symbol "MODE1/2048"
        , CueTrackMode1_2352 <$ symbol "MODE1/2352"
        , CueTrackMode2_2336 <$ symbol "MODE2/2336"
        , CueTrackMode2_2352 <$ symbol "MODE2/2352"
        , CueTrackCdi2336    <$ symbol "CDI/2336"
        , CueTrackCdi2352    <$ symbol "CDI/2352" ]
  trackType <- pTrackType <* eol <* scn
  let newTrack = dummyTrack { cueTrackType = trackType }
  modify $ \x -> x
    { contextTracks     = newTrack : contextTracks x
    , contextTrackCount = trackCount + 1
    , contextCueSheet   = let old = contextCueSheet x in
        if firstTrack
          then old { cueFirstTrackNumber = n }
          else old }
  inTrack n $ do
    void (many pTrackHeaderItem)
    -- TODO optional index 0
    -- TODO indices
    void (optional pPostgap)

pTrackHeaderItem :: Parser ()
pTrackHeaderItem = choice
  [ pFlags
  , pIsrc
  , pTrackPerformer
  , pTrackTitle
  , pTrackSongwriter
  , pRem
  , pPregap ]

pFlags :: Parser ()
pFlags = do
  already <- gets (seenFlags . head . contextTracks)
  let f () =
        if already
          then Left CueParserDuplicateTrackFlags
          else Right ()
  withCheck f (void $ symbol "FLAGS")
  void (some pFlag) <* eol <* scn

-- | A helper data type.

data CueTrackFlag = DCP | FourCH | PRE | SCMS

pFlag :: Parser ()
pFlag = do
  flag <- choice
    [ DCP    <$ symbol "DCP"
    , FourCH <$ symbol "4CH"
    , PRE    <$ symbol "PRE"
    , SCMS   <$ symbol "SCMS" ]
  modify $ \x -> x
    { contextTracks = changingFirstOf (contextTracks x) $ \t ->
        case flag of
          DCP    -> t { cueTrackDigitalCopyPermitted = True }
          FourCH -> t { cueTrackFourChannelAudio     = True }
          PRE    -> t { cueTrackPreemphasisEnabled   = True }
          SCMS   -> t { cueTrackSerialCopyManagement = True } }

pIsrc :: Parser ()
pIsrc = do
  already <- bool Nothing (Just CueParserDuplicateTrackIsrc)
    <$> gets (isJust . cueTrackIsrc . head . contextTracks)
  let f x' = let x = T.pack x' in
        case mkIsrc x of
          Nothing -> Left (CueParserInvalidTrackIsrc x)
          Just isrc -> Right isrc
  isrc <- labelledLit already f "ISRC"
  modify $ \x -> x
    { contextTracks = changingFirstOf (contextTracks x) $ \t ->
        t { cueTrackIsrc = Just isrc } }

pTrackPerformer :: Parser ()
pTrackPerformer = do
  already <- bool Nothing (Just CueParserDuplicateTrackPerformer)
    <$> gets (isJust . cueTrackPerformer . head . contextTracks)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  performer <- labelledLit already f "PERFORMER"
  modify $ \x -> x
    { contextTracks = changingFirstOf (contextTracks x) $ \t ->
        t { cueTrackPerformer = Just performer } }

pTrackTitle :: Parser ()
pTrackTitle = do
  already <- bool Nothing (Just CueParserDuplicateTrackTitle)
    <$> gets (isJust . cueTrackTitle . head . contextTracks)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  title <- labelledLit already f "TITLE"
  modify $ \x -> x
    { contextTracks = changingFirstOf (contextTracks x) $ \t ->
        t { cueTrackTitle = Just title } }

pTrackSongwriter :: Parser ()
pTrackSongwriter = do
  already <- bool Nothing (Just CueParserDuplicateTrackSongwriter)
    <$> gets (isJust . cueTrackSongwriter . head . contextTracks)
  let f x' = let x = T.pack x' in
        case mkCueText x of
          Nothing -> Left (CueParserInvalidCueText x)
          Just txt -> Right txt
  songwriter <- labelledLit already f "SONGWRITER"
  modify $ \x -> x
    { contextTracks = changingFirstOf (contextTracks x) $ \t ->
        t { cueTrackSongwriter = Just songwriter } }

pPregap :: Parser ()
pPregap = undefined -- TODO

pPostgap :: Parser ()
pPostgap = undefined -- TODO

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

-- | A labelled literal (a helper for common case).

labelledLit
  :: Maybe CueParserFailure -- ^ Which error to signal when command is parsed
  -> (String -> Either CueParserFailure a)
  -> String
  -> Parser a
labelledLit mfail check command = do
  let f () =
        case mfail of
          Nothing -> Right ()
          Just err -> Left err
  withCheck f (void $ symbol command)
  withCheck check (lexeme stringLit) <* eol <* scn

-- | String literal with support for quotation.

stringLit :: Parser String
stringLit = quoted <|> unquoted
  where
    quoted   = char '\"' *> manyTill (noneOf ("\n" :: String)) (char '\"')
    unquoted = many (noneOf ("\n\t " :: String))

-- | Case-insensitive symbol parser.

symbol :: String -> Parser String
symbol s = string' s <* notFollowedBy alphaNumChar <* sc

-- | A wrapper for lexemes.

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

-- | Space consumer (eats newlines).

scn :: Parser ()
scn = L.space (void spaceChar) empty empty

-- | Space consumer (does not eat newlines).

sc :: Parser ()
sc = L.space (void $ oneOf ("\t " :: String)) empty empty

-- | Determine by 'CueTrack' if we have already parsed FLAGS command.

seenFlags :: CueTrack -> Bool
seenFlags CueTrack {..} = or
  [ cueTrackDigitalCopyPermitted
  , cueTrackFourChannelAudio
  , cueTrackPreemphasisEnabled
  , cueTrackSerialCopyManagement ]

-- | Apply given function to the first element of the list.

changingFirstOf :: [a] -> (a -> a) -> [a]
changingFirstOf [] _ = []
changingFirstOf (x:xs) f = f x : xs

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
