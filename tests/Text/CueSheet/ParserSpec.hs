{-# LANGUAGE OverloadedStrings #-}

module Text.CueSheet.ParserSpec
  ( spec )
where

import Control.Monad.Catch
import Data.Char (ord)
import Data.Monoid ((<>))
import Data.Word (Word8)
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.CueSheet.Parser
import Text.CueSheet.Types
import Text.Megaparsec
import qualified Data.ByteString    as B
import qualified Data.List.NonEmpty as NE

spec :: Spec
spec =
  describe "parseCueSheet" $ do
    it "correctly deals with whitespace" $ do
      bs <- B.readFile "cue-sheet-samples/parser-whitespace.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "parses commands case-insensitively" $ do
      bs <- B.readFile "cue-sheet-samples/parser-case-insensitivity.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "handles comments properly" $ do
      bs <- B.readFile "cue-sheet-samples/parser-comments.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "at least one file should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-file.cue"
      let es = foldMap (etoks . tow)
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        err (posN (70 :: Int) bs) (ueof <> es)
    it "at least one track should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-track.cue"
      parseCueSheet "" bs `shouldFailWith`
        err (posN (110 :: Int) bs) (ueof <> etoks (tow "REM") <> etoks (tow "TRACK"))
    it "at least one non-zero index should be declared" $ do
      bs <- B.readFile "cue-sheet-samples/parser-missing-index.cue"
      let e = wrappedTrivial (ueof <> etoks (tow "INDEX") <> etoks (tow "REM"))
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (180 :: Int) bs) (cstm (Eec (Just 1) e))
    it "parses a normal CUE file without issues" $ do
      bs <- B.readFile "cue-sheet-samples/parser-normal.cue"
      sheet <- normalCueSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "rejects invalid catalog number" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-catalog.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (8 :: Int) bs)
        (cstm (Eec Nothing (CueParserInvalidCatalog "123")))
    it "rejects invalid CUE text" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-text.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (10 :: Int) bs)
        (cstm (Eec Nothing (CueParserInvalidCueText "")))
    it "detects and reports tracks that appear out of order" $ do
      bs <- B.readFile "cue-sheet-samples/parser-track-out-of-order.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (182 :: Int) bs)
        (cstm (Eec Nothing CueParserTrackOutOfOrder))
    it "rejects invalid ISRC values" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-isrc.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (161 :: Int) bs)
        (cstm (Eec (Just 3) (CueParserInvalidTrackIsrc "123")))
    it "rejects invalid number of seconds" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-seconds.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (168 :: Int) bs)
        (cstm (Eec (Just 1) (CueParserInvalidSeconds 61)))
    it "rejects invalid number of frames" $ do
      bs <- B.readFile "cue-sheet-samples/parser-invalid-frames.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (171 :: Int) bs)
        (cstm (Eec (Just 1) (CueParserInvalidFrames 77)))
    it "detects and reports indices that appear out of order" $ do
      bs <- B.readFile "cue-sheet-samples/parser-index-out-of-order.cue"
      parseCueSheet "" bs `shouldFailWith` errFancy (posN (162 :: Int) bs)
        (cstm (Eec (Just 1) CueParserTrackIndexOutOfOrder))
    it "rejects duplicate CATALOG command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-catalog.cue"
      let es = utoks (tow "CATA") <> foldMap (etoks . tow)
            [ "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (22 :: Int) bs) es
    it "rejects duplicate CDTEXTFILE command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-cdtextfile.cue"
      let es = utoks (tow "CDTE") <> foldMap (etoks . tow)
            [ "CATALOG"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (23 :: Int) bs) es
    it "rejects duplicate PERFORMER command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-performer.cue"
      let es = utoks (tow "PERF") <> foldMap (etoks . tow)
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (30 :: Int) bs) es
    it "rejects duplicate TITLE command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-title.cue"
      let es = utoks (tow "TITL") <> foldMap (etoks . tow)
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (17 :: Int) bs) es
    it "rejects duplicate SONGWRITER command" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-songwriter.cue"
      let es = utoks (tow "SONG") <> foldMap (etoks . tow)
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (18 :: Int) bs) es
    it "rejects duplicate FLAGS command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-flags.cue"
      let es = wrappedTrivial $ utoks (tow "FLAGS") <> foldMap (etoks . tow)
            [ "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (144 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate ISRC command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-isrc.cue"
      let es = wrappedTrivial $ utoks (tow "ISRC ") <> foldMap (etoks . tow)
            [ "FLAGS"
            , "INDEX"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (152 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate PERFORMER command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-performer.cue"
      let es = wrappedTrivial $ utoks (tow "PERFO") <> foldMap (etoks . tow)
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (156 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate TITLE command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-title.cue"
      let es = wrappedTrivial $ utoks (tow "TITLE") <> foldMap (etoks . tow)
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (146 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate SONGWRITER command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-songwriter.cue"
      let es = wrappedTrivial $ utoks (tow "SONGW") <> foldMap (etoks . tow)
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (150 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate PREGAP command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-pregap.cue"
      let es = wrappedTrivial $ utoks (tow "PREGA") <> foldMap (etoks . tow)
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith`
        errFancy (posN (176 :: Int) bs) (cstm (Eec (Just 1) es))
    it "rejects duplicate POSTGAP command (in track)" $ do
      bs <- B.readFile "cue-sheet-samples/parser-duplicate-track-postgap.cue"
      let es = foldMap (etoks . tow)
            [ "FILE"
            , "REM"
            , "TRACK" ]
      parseCueSheet "" bs `shouldFailWith`
        err (posN (199 :: Int) bs) (utok 80 <> es <> eeof)

testSheet :: MonadThrow m => m CueSheet
testSheet = do
  performer  <- mkCueText "Faithless"
  title      <- mkCueText "Live in Berlin"
  trackTitle <- mkCueText "Reverence"
  return CueSheet
    { cueCatalog    = Nothing
    , cueCdTextFile = Nothing
    , cuePerformer  = Just performer
    , cueTitle      = Just title
    , cueSongwriter = Nothing
    , cueFirstTrackNumber = 1
    , cueFiles      = NE.fromList
      [ CueFile
        { cueFileName = "Faithless - Live in Berlin.mp3"
        , cueFileType = MP3
        , cueFileTracks = NE.fromList
          [ CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just trackTitle
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList
              [CueTime 3, CueTime 151]
            , cueTrackPostgap              = Nothing } ] } ] }

normalCueSheet :: MonadThrow m => m CueSheet
normalCueSheet = do
  performer   <- mkCueText "Faithless"
  catalog     <- mkMcn "1112223334445"
  title       <- mkCueText "Live in Berlin"
  track1Title <- mkCueText "Reverence"
  track2Title <- mkCueText "She's My Baby"
  track3Title <- mkCueText "Take the Long Way Home"
  track4Title <- mkCueText "Insomnia"
  track5Title <- mkCueText "Bring the Family Back"
  track6Title <- mkCueText "Salva Mea"
  track7Title <- mkCueText "Dirty Old Man"
  track8Title <- mkCueText "God Is a DJ"
  return CueSheet
    { cueCatalog    = Just catalog
    , cueCdTextFile = Just "blah-blah.txt"
    , cuePerformer  = Just performer
    , cueTitle      = Just title
    , cueSongwriter = Nothing
    , cueFirstTrackNumber = 1
    , cueFiles      = NE.fromList
      [ CueFile
        { cueFileName = "Faithless - Live in Berlin.mp3"
        , cueFileType = MP3
        , cueFileTracks = NE.fromList
          [ CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track1Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Just (CueTime 75)
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 0]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track2Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 30150]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track3Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 49050]
            , cueTrackPostgap              = Just (CueTime 76) }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track4Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList
              [CueTime 76800, CueTime 76805]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = True
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track5Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 115800]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track6Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 138750]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track7Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 172800]
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Nothing
            , cueTrackTitle                = Just track8Title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = NE.fromList [CueTime 191625]
            , cueTrackPostgap              = Nothing } ] } ] }

-- | Construct 'CueParserFailure' from given @'ET' 'Word8'@ thing.

wrappedTrivial :: ET Word8 -> CueParserFailure
wrappedTrivial xs =
  case err posI xs of
    TrivialError _ us es -> CueParserTrivialError us es
    _                    -> error "Ooops!"

-- | Convert 'String' to a list of bytes.

tow :: String -> [Word8]
tow = fmap (fromIntegral . ord)
{-# INLINE tow #-}

-- | Quickly make a custom error component.

cstm :: e -> EF e
cstm = fancy . ErrorCustom
{-# INLINE cstm #-}
