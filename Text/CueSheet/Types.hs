-- |
-- Module      :  Text.CueSheet.Types
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types describing structure of a CUE sheet. You probably want to import
-- "Text.CueSheet" instead.

{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Text.CueSheet.Types
  ( CueSheet      (..)
  , CueFile       (..)
  , CueFileType   (..)
  , CueTrack      (..)
  , CueTrackType  (..)
  , CueTime       (..) )
where

import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Numeric.Natural

-- TODO Smart constructors for MCN, ISRC, and text fields that must be
-- between 1 and 80 chars long.

-- | Entire CUE sheet, contains one or more files (see 'CueFile').

data CueSheet = CueSheet
  { cueCatalog    :: Maybe Text
    -- ^ Disc's Media Catalog Number (MCN), must be 13 characters long.
  , cueCdTextFile :: Maybe String
    -- ^ Name of the file that contains the encoded CD-Text information for
    -- the disc.
  , cueTitle      :: Maybe Text
    -- ^ Title of entire disc. Must be from 1 to 80 characters long.
  , cuePerformer  :: Maybe Text
    -- ^ Performer of entire disc. Must be from 1 to 80 characters long.
  , cueSongwriter :: Maybe Text
    -- ^ Songwriter of entire disc. Must be from 1 to 80 characters long.
  , cueFristTrackNumber :: Natural
    -- ^ Number of first track. Typically 1, but may be greater than 1.
  , cueFiles      :: NonEmpty CueFile
    -- ^ Collection of files to be written.
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | A file to be written. Single file can be divided into one or more
-- tracks (see 'CueTrack').

data CueFile = CueFile
  { cueFileName   :: String            -- ^ Name of file.
  , cueFileType   :: CueFileType       -- ^ Type of file.
  , cueFileTracks :: NonEmpty CueTrack -- ^ Collection of tracks in the file.
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | Enumeration of audio or file's data types.

data CueFileType
  = Binary
    -- ^ Intel binary file (least significant byte first). Use for data
    -- files.
  | Motorola
    -- ^ Motorola binary file (most significant file first). Use for data
    -- files.
  | Aiff
    -- ^ Audio AIFF file (44.1 kHz, 16 bit stereo).
  | Wave
    -- ^ Audio WAVE file (44.1 kHz, 16 bit stereo).
  | MP3
    -- ^ Audio MP3 file (44.1 kHz 16 bit stereo).
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

-- | A track. Single track can have one or more indices (see
-- 'CueTrackIndex').

data CueTrack = CueTrack
  { cueTrackDigitalCopyPermitted :: Bool
    -- ^ Flag: digital copy permitted.
  , cueTrackFourChannelAudio     :: Bool
    -- ^ Flag: four channel audio.
  , cueTrackPreemphasisEnabled   :: Bool
    -- ^ Flag: pre-emphasis enabled (audio track only).
  , cueTrackSerialCopyManagement :: Bool
    -- ^ Flag: serial copy management system (not supported by all
    -- recorders).
  , cueTrackType                 :: CueTrackType
    -- ^ Type datatype.
  , cueTrackIsrc                 :: Maybe Text
    -- ^ The track's International Standard Recording Code (ISRC). It must
    -- be 12 characters in length. The first five characters are
    -- alphanumeric, the last seven are numeric only.
  , cueTrackTitle                :: Maybe Text
    -- ^ Title of the track. Must be from 1 to 80 characters long.
  , cueTrackPerformer            :: Maybe Text
    -- ^ Performer of the track. Must be from 1 to 80 characters long.
  , cueTrackSongwriter           :: Maybe Text
    -- ^ Songwriter of the track. Must be from 1 to 80 characters long.
  , cueTrackPregap               :: Maybe CueTime
    -- ^ Track's pregap.
  , cueTrackPregapIndex          :: Maybe CueTime
    -- ^ Starting time of track pregap, a.k.a. INDEX 0.
  , cueTrackIndicies             :: NonEmpty CueTime
    -- ^ Collection of indices for the track starting with index 1. The
    -- index specifies the starting time of the track data. Index 1 is the
    -- only index that's stored in the disc's table of contents.
  , cueTrackPostgap              :: Maybe CueTime
    -- ^ Track's postgap.
  } deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- | Track datatype.

data CueTrackType
  = CueTrackAudio      -- ^ Audio\/Music (2352).
  | CueTrackCdg        -- ^ Karaoke CD+G (2448).
  | CueTrackMode1_2048 -- ^ CD-ROM Mode1 data (cooked).
  | CueTrackMode1_2352 -- ^ CD-ROM Mode1 data (raw).
  | CueTrackMode2_2336 -- ^ CD-ROM XA Mode2 data.
  | CueTrackMode2_2352 -- ^ CD-ROM XA Mode2 data.
  | CueTrackCdi2336    -- ^ CD-I Mode2 data.
  | CueTrackCdi2352    -- ^ CD-I Mode2 data.
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Data, Typeable, Generic)

-- | This datatype is used to indicate duration and position in time. It
-- contains number of frames. There is 75 frames in one second.

newtype CueTime = CueTime Natural
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

-- TODO Extract number of minutes, seconds, and frames from CueTrackIndex.
-- TODO Pretty print/show the stuff.
