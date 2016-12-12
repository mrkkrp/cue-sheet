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

import Control.Monad.State.Strict
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Typeable (Typeable)
import GHC.Generics
import Numeric.Natural
import Text.CueSheet.Types
import Text.Megaparsec
import qualified Data.ByteString.Lazy as BL
import qualified Data.Set             as E

----------------------------------------------------------------------------
-- Types

-- | Extended error component with support for storing number of track
-- declaration in which a parsing error has occured.

data Eec = Eec (Maybe Natural) (Maybe CueParserFailure)
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

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
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance ShowErrorComponent CueParserFailure where
  showErrorComponent = \case
    CueParserFail msg ->
      showErrorComponent (DecFail msg)
    CueParserIndentation ord p0 p1 ->
      showErrorComponent (DecIndentation ord p0 p1)

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
pCueSheet = undefined -- TODO

----------------------------------------------------------------------------
-- Helpers

-- | Parse a thing and then check if it's OK conceptually. If it's not OK,
-- the error will be reported with position at the start of offending
-- lexeme, otherwise the lexeme is parsed as usual. Of course if the lexeme
-- has incorrect format, that is just reported and no additional check
-- happens.

withCheck :: (a -> Either CueParserFailure a) -> Parser a -> Parser a
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
