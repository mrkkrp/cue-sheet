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

{-# LANGUAGE CPP                #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}

module Text.CueSheet.Types
  ( CueSheet      (..)
  , CueFile       (..)
  , CueFileType   (..)
  , CueTrack      (..)
  , CueTrackType  (..)
  , CueTime       (..)
  , fromMmSsFf
  , toMmSsFf
  , showMmSsFf
  , Mcn
  , mkMcn
  , unMcn
  , CueText
  , mkCueText
  , unCueText
  , Isrc
  , mkIsrc
  , unIsrc
  , CueSheetException (..) )
where

import Control.Monad.Catch
import Data.Char (isDigit, isAscii, isLetter)
import Data.Data (Data)
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid ((<>))
import Data.Text (Text)
import Data.Typeable (Typeable)
import GHC.Generics
import Numeric.Natural
import Test.QuickCheck
import Text.Printf (printf)
import qualified Data.List.NonEmpty as NE
import qualified Data.Text          as T

-- | Entire CUE sheet, contains one or more files (see 'CueFile').

data CueSheet = CueSheet
  { cueCatalog    :: !(Maybe Mcn)
    -- ^ Disc's Media Catalog Number (see 'Mcn').
  , cueCdTextFile :: !(Maybe String)
    -- ^ Name of the file that contains the encoded CD-Text information for
    -- the disc.
  , cueTitle      :: !(Maybe CueText)
    -- ^ Title of the entire disc.
  , cuePerformer  :: !(Maybe CueText)
    -- ^ Performer of the entire disc.
  , cueSongwriter :: !(Maybe CueText)
    -- ^ Songwriter of the entire disc.
  , cueFristTrackNumber :: !Natural
    -- ^ Number of the first track. Typically 1, but may be greater than 1.
  , cueFiles      :: !(NonEmpty CueFile)
    -- ^ Collection of files to be written.
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary CueSheet where
  arbitrary = CueSheet
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
#if MIN_VERSION_QuickCheck(2,9,0)
    <*> arbitrary
#else
    <*> (NE.fromList . getNonEmpty <$> arbitrary)
#endif

-- | A file to be written. Single file can be divided into one or more
-- tracks (see 'CueTrack').

data CueFile = CueFile
  { cueFileName   :: !String
    -- ^ Name of file.
  , cueFileType   :: !CueFileType
    -- ^ Type of file.
  , cueFileTracks :: !(NonEmpty CueTrack)
    -- ^ Collection of tracks in the file.
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary CueFile where
  arbitrary = CueFile
    <$> arbitrary
    <*> arbitrary
#if MIN_VERSION_QuickCheck(2,9,0)
    <*> arbitrary
#else
    <*> (NE.fromList . getNonEmpty <$> arbitrary)
#endif

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

instance Arbitrary CueFileType where
  arbitrary = elements [minBound..maxBound]

-- | A track. Single track can have one or more indices.

data CueTrack = CueTrack
  { cueTrackDigitalCopyPermitted :: !Bool
    -- ^ Flag: digital copy permitted.
  , cueTrackFourChannelAudio     :: !Bool
    -- ^ Flag: four channel audio.
  , cueTrackPreemphasisEnabled   :: !Bool
    -- ^ Flag: pre-emphasis enabled (audio track only).
  , cueTrackSerialCopyManagement :: !Bool
    -- ^ Flag: serial copy management system (not supported by all
    -- recorders).
  , cueTrackType                 :: !CueTrackType
    -- ^ Type datatype.
  , cueTrackIsrc                 :: !(Maybe Isrc)
    -- ^ The track's International Standard Recording Code (ISRC).
  , cueTrackTitle                :: !(Maybe CueText)
    -- ^ Title of the track.
  , cueTrackPerformer            :: !(Maybe CueText)
    -- ^ Performer of the track.
  , cueTrackSongwriter           :: !(Maybe CueText)
    -- ^ Songwriter of the track.
  , cueTrackPregap               :: !(Maybe CueTime)
    -- ^ Track's pregap.
  , cueTrackPregapIndex          :: !(Maybe CueTime)
    -- ^ Starting time of track pregap, a.k.a. INDEX 0.
  , cueTrackIndicies             :: !(NonEmpty CueTime)
    -- ^ Collection of indices for the track starting with index 1. The
    -- index specifies the starting time of the track data. Index 1 is the
    -- only index that's stored in the disc's table of contents.
  , cueTrackPostgap              :: !(Maybe CueTime)
    -- ^ Track's postgap.
  } deriving (Show, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary CueTrack where
  arbitrary = CueTrack
    <$> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
#if MIN_VERSION_QuickCheck(2,9,0)
    <*> arbitrary
#else
    <*> (NE.fromList . getNonEmpty <$> arbitrary)
#endif
    <*> arbitrary

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

instance Arbitrary CueTrackType where
  arbitrary = elements [minBound..maxBound]

-- | This datatype is used to indicate duration and position in time. It
-- contains number of frames. There is 75 frames in one second.

newtype CueTime = CueTime Natural
  deriving (Show, Read, Eq, Ord, Data, Typeable, Generic)

instance Arbitrary CueTime where
  arbitrary = CueTime <$> arbitrary

-- | Construct 'CueTime' from minutes, seconds, and frames. There are 75
-- frames per second. If number of seconds or frames is invalid,
-- 'InvalidSeconds' or 'InvalidFrames' will be thrown.

fromMmSsFf :: MonadThrow m
  => Natural           -- ^ Number of minutes, no limit here
  -> Natural           -- ^ Number of seconds, 0–59 inclusive
  -> Natural           -- ^ Number of frames, 0–74 inclusive
  -> m CueTime         -- ^ The result
fromMmSsFf mm ss ff
  | ss >= 60  = throwM (InvalidSeconds ss)
  | ff >= 75  = throwM (InvalidFrames ff)
  | otherwise =
    let ss' = mm * 60 + ss
        ff' = ss' * 75 + ff
    in return (CueTime ff')

-- | Get minutes, seconds, and frames from a 'CueTime' value.

toMmSsFf :: CueTime -> (Natural, Natural, Natural)
toMmSsFf (CueTime ff') = (mm,ss,ff)
  where
    (ss', ff) = ff' `quotRem` 75
    (mm,  ss) = ss' `quotRem` 60

-- | Render representation of 'CueTime' in @mm:ss:ff@ format.

showMmSsFf :: CueTime -> Text
showMmSsFf x = T.pack (printf "%02d:%02d:%02d" mm ss ff)
  where
    (mm,ss,ff) = toMmSsFf x

-- | Disc's Media Catalog Number (MCN), must be 13 characters long, all the
-- characters must be numeric.

newtype Mcn = Mcn Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show Mcn where
  show = show . unMcn

instance Arbitrary Mcn where
  arbitrary = Mcn <$> ((T.pack <$> arbitrary) `suchThat` isValidMcn)

-- | Make a 'Mcn'. If the provided 'Text' value is not a valid MCN, throw
-- the 'InvalidMcnException'.

mkMcn :: MonadThrow m => Text -> m Mcn
mkMcn x =
  if isValidMcn x
    then return (Mcn x)
    else throwM (InvalidMcnException x)

-- | Get 'Text' from 'Mcn'.

unMcn :: Mcn -> Text
unMcn (Mcn x) = x

-- | A type for things like title or performer that should have length
-- between 1 and 80 characters as per spec.

newtype CueText = CueText Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show CueText where
  show = show . unCueText

instance Arbitrary CueText where
  arbitrary = CueText <$> ((T.pack <$> arbitrary) `suchThat` isValidCueText)

-- | Make a 'CueText'. If the provided 'Text' value is not a valid CUE text,
-- throw the 'InvalidCueText' exception.

mkCueText :: MonadThrow m => Text -> m CueText
mkCueText x =
  if isValidCueText x
    then return (CueText x)
    else throwM (InvalidCueText x)

-- | Get 'Text' from 'CueText'.

unCueText :: CueText -> Text
unCueText (CueText x) = x

-- | The track's International Standard Recording Code (ISRC). It must be 12
-- characters in length. The first five characters are alphanumeric, the
-- last seven are numeric only.

newtype Isrc = Isrc Text
  deriving (Eq, Ord, Data, Typeable, Generic)

instance Show Isrc where
  show = show . unIsrc

instance Arbitrary Isrc where
  arbitrary = do
    pre <- vectorOf 5 (arbitrary `suchThat` isAlphaNum)
    post <- vectorOf 7 (arbitrary `suchThat` isDigit)
    (return . Isrc . T.pack) (pre <> post)

-- | Make a 'Isrc', if the provided 'Text' value is not a valid ISRC, throw
-- the 'InvalidIsrc' exception.

mkIsrc :: MonadThrow m => Text -> m Isrc
mkIsrc x =
  if T.length x == 12              &&
     T.all isAlphaNum (T.take 5 x) &&
     T.all isDigit (T.drop 5 x)
    then return (Isrc x)
    else throwM (InvalidIsrc x)

-- | Get 'Text' from 'Isrc'.

unIsrc :: Isrc -> Text
unIsrc (Isrc x) = x

-- | Exception type for bad things that may happen while you use the
-- library.

data CueSheetException
  = InvalidSeconds Natural
    -- ^ The value is greater than 59 and thus is invalid for 'fromMmSsFf'.
  | InvalidFrames Natural
    -- ^ The value is greater than 74 and thus is invalid for 'fromMmSsFf'.
  | InvalidMcnException Text
    -- ^ Provided text wasn't a correct media catalog number (MCN).
  | InvalidCueText Text
    -- ^ Provided text wasn't a valid CUE text.
  | InvalidIsrc Text
    -- ^ Provided text wasn't a valid ISRC.
  deriving (Eq, Ord, Show, Read, Data, Typeable, Generic)

instance Exception CueSheetException

----------------------------------------------------------------------------
-- Helpers

-- | Check if given 'Text' is a valid MCN.

isValidMcn :: Text -> Bool
isValidMcn x = T.length x == 13 && T.all isDigit x

-- | Check if given 'Text' has valid length to be used in a CUE sheet as
-- performer, title, etc.

isValidCueText :: Text -> Bool
isValidCueText x = l >= 1 && l <= 80
  where
    l = T.length x

-- | A variant of 'Data.Char.IsAlphaNum' that only permits ASCII letter
-- chars.

isAlphaNum :: Char -> Bool
isAlphaNum a = isAscii a && (isDigit a || isLetter a)
