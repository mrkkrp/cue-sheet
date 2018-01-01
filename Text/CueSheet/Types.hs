-- |
-- Module      :  Text.CueSheet.Types
-- Copyright   :  © 2016–2018 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov92@gmail.com>
-- Stability   :  experimental
-- Portability :  portable
--
-- Types describing structure of a CUE sheet. You probably want to import
-- "Text.CueSheet" instead.

{-# LANGUAGE CPP           #-}
{-# LANGUAGE DeriveGeneric #-}

module Text.CueSheet.Types
  ( CueSheet (..)
  , CueFile (..)
  , CueFileType (..)
  , CueTrack (..)
  , CueTrackType (..)
  , CueTime (..)
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
import Data.List.NonEmpty (NonEmpty (..))
import Data.Monoid ((<>))
import Data.Text (Text)
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
  , cueCdTextFile :: !(Maybe FilePath)
    -- ^ Name of the file that contains the encoded CD-Text information for
    -- the disc.
  , cuePerformer  :: !(Maybe CueText)
    -- ^ Performer of the entire disc.
  , cueTitle      :: !(Maybe CueText)
    -- ^ Title of the entire disc.
  , cueSongwriter :: !(Maybe CueText)
    -- ^ Songwriter of the entire disc.
  , cueFirstTrackNumber :: !Natural
    -- ^ Number of the first track. Typically 1, but may be greater than 1.
  , cueFiles      :: !(NonEmpty CueFile)
    -- ^ Collection of files to be written.
  } deriving (Show, Eq, Ord, Generic)

instance Arbitrary CueSheet where
  arbitrary = CueSheet
    <$> arbitrary
    <*> oneof [pure Nothing, Just <$> filepath]
    <*> arbitrary
    <*> arbitrary
    <*> arbitrary
    <*> (fromInteger . getPositive <$> arbitrary)
    <*> scaleDown (NE.fromList . getNonEmpty <$> arbitrary)

-- | A file to be written. Single file can be divided into one or more
-- tracks (see 'CueTrack').

data CueFile = CueFile
  { cueFileName   :: !FilePath
    -- ^ Name of file.
  , cueFileType   :: !CueFileType
    -- ^ Type of file.
  , cueFileTracks :: !(NonEmpty CueTrack)
    -- ^ Collection of tracks in the file.
  } deriving (Show, Eq, Ord, Generic)

instance Arbitrary CueFile where
  arbitrary = CueFile
    <$> filepath
    <*> arbitrary
    <*> scaleDown (NE.fromList . getNonEmpty <$> arbitrary)

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
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

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
  , cueTrackIndices              :: !(NonEmpty CueTime)
    -- ^ Collection of indices for the track starting with index 1. The
    -- index specifies the starting time of the track data. Index 1 is the
    -- only index that's stored in the disc's table of contents.
  , cueTrackPostgap              :: !(Maybe CueTime)
    -- ^ Track's postgap.
  } deriving (Show, Eq, Ord, Generic)

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
    <*> scaleDown (NE.fromList . getNonEmpty <$> arbitrary)
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
  deriving (Show, Read, Eq, Ord, Bounded, Enum, Generic)

instance Arbitrary CueTrackType where
  arbitrary = elements [minBound..maxBound]

-- | This datatype is used to indicate duration and position in time. It
-- contains number of frames. There are 75 frames in one second.

newtype CueTime = CueTime Natural
  deriving (Show, Read, Eq, Ord, Generic)

instance Arbitrary CueTime where
  arbitrary = CueTime . fromInteger . getNonNegative <$> arbitrary

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
  deriving (Eq, Ord, Generic)

instance Show Mcn where
  show = show . unMcn

instance Arbitrary Mcn where
  arbitrary = Mcn . T.pack <$> vectorOf 13 (arbitrary `suchThat` isDigit)

-- | Make a 'Mcn'. If the provided 'Text' value is not a valid MCN, throw
-- the 'InvalidMcnException'.

mkMcn :: MonadThrow m => Text -> m Mcn
mkMcn x =
  if isValidMcn x
    then return (Mcn x)
    else throwM (InvalidMcn x)

-- | Get 'Text' from 'Mcn'.

unMcn :: Mcn -> Text
unMcn (Mcn x) = x

-- | A type for things like title or performer that should have length
-- between 1 and 80 characters as per spec. We also demand that it does not
-- contain @\"@ and newline characters, as it's not clear from the spec how
-- to escape them properly.

newtype CueText = CueText Text
  deriving (Eq, Ord, Generic)

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
  deriving (Eq, Ord, Generic)

instance Show Isrc where
  show = show . unIsrc

instance Arbitrary Isrc where
  arbitrary = do
    pre <- vectorOf 5 (arbitrary `suchThat` isAlphaNum)
    post <- vectorOf 7 (arbitrary `suchThat` isDigit)
    (return . Isrc . T.pack) (pre <> post)

-- | Make an 'Isrc', if the provided 'Text' value is not a valid ISRC, throw
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

-- | Exception type for the bad things that may happen while you use the
-- library.

data CueSheetException
  = InvalidSeconds Natural
    -- ^ The value is greater than 59 and thus is invalid for 'fromMmSsFf'.
  | InvalidFrames Natural
    -- ^ The value is greater than 74 and thus is invalid for 'fromMmSsFf'.
  | InvalidMcn Text
    -- ^ Provided text wasn't a correct media catalog number (MCN).
  | InvalidCueText Text
    -- ^ Provided text wasn't a valid CUE text.
  | InvalidIsrc Text
    -- ^ Provided text wasn't a valid ISRC.
  deriving (Eq, Ord, Show, Read, Generic)

instance Exception CueSheetException

----------------------------------------------------------------------------
-- Helpers

-- | Check if the given 'Text' is a valid MCN.

isValidMcn :: Text -> Bool
isValidMcn x = T.length x == 13 && T.all isDigit x

-- | Check if the given 'Text' has valid length and contents to be used in a
-- CUE sheet as performer, title, etc.

isValidCueText :: Text -> Bool
isValidCueText x = l >= 1 && l <= 80 && T.all f x
  where
    l = T.length x
    f c = c /= '\"' && c /= '\n'

-- | A variant of 'Data.Char.IsAlphaNum' that only permits ASCII letter
-- chars.

isAlphaNum :: Char -> Bool
isAlphaNum a = isAscii a && (isDigit a || isLetter a)

-- | Scale down size of 'arbitrary'-generated stuff.

scaleDown :: Gen a -> Gen a
scaleDown = scale (`quot` 3)

-- | File path generator.

filepath :: Gen FilePath
filepath = listOf (arbitrary `suchThat` windowsLikesIt)
  where
    windowsLikesIt = (`notElem` "?%*:<>#|\"\\\n")
