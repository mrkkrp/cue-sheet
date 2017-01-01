--
-- Parser tests for the ‘cue-sheet’ package.
--
-- Copyright © 2016–2017 Mark Karpov <markkarpov@openmailbox.org>
--
-- Redistribution and use in source and binary forms, with or without
-- modification, are permitted provided that the following conditions are
-- met:
--
-- * Redistributions of source code must retain the above copyright notice,
--   this list of conditions and the following disclaimer.
--
-- * Redistributions in binary form must reproduce the above copyright
--   notice, this list of conditions and the following disclaimer in the
--   documentation and/or other materials provided with the distribution.
--
-- * Neither the name Mark Karpov nor the names of contributors may be used
--   to endorse or promote products derived from this software without
--   specific prior written permission.
--
-- THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS “AS IS” AND ANY
-- EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
-- WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
-- DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDERS BE LIABLE FOR ANY
-- DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL
-- DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS
-- OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
-- HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT,
-- STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN
-- ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE
-- POSSIBILITY OF SUCH DAMAGE.

{-# LANGUAGE OverloadedStrings #-}

module Text.CueSheet.ParserSpec
  ( spec )
where

import Control.Monad.Catch
import Data.Monoid ((<>))
import Test.Hspec
import Test.Hspec.Megaparsec
import Text.CueSheet.Parser
import Text.CueSheet.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty   as NE

spec :: Spec
spec =
  describe "parseCueSheet" $ do
    it "correctly deals with whitespace" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-whitespace.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "parses commands case-insensitively" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-case-insensitivity.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "handles comments properly" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-comments.cue"
      sheet <- testSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "at least one file should be declared" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-missing-file.cue"
      let es = foldMap etoks
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
      bs <- BL.readFile "cue-sheet-samples/parser-missing-track.cue"
      parseCueSheet "" bs `shouldFailWith`
        err (posN (110 :: Int) bs) (ueof <> etoks "REM" <> etoks "TRACK")
    it "at least one non-zero index should be declared" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-missing-index.cue"
      let inTrack1 = cstm (Eec (Just 1) Nothing)
      parseCueSheet "" bs `shouldFailWith`
        err (posN (180 :: Int) bs)
          (ueof <> etoks "INDEX" <> etoks "REM" <> inTrack1)
    it "parses a normal CUE file without issues" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-normal.cue"
      sheet <- normalCueSheet
      parseCueSheet "" bs `shouldParse` sheet
    it "rejects invalid catalog number" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-invalid-catalog.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (8 :: Int) bs)
        (cstm (Eec Nothing (Just $ CueParserInvalidCatalog "123")))
    it "rejects invalid CUE text" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-invalid-text.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (10 :: Int) bs)
        (cstm (Eec Nothing (Just $ CueParserInvalidCueText "")))
    it "detects and reports tracks that appear out of order" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-track-out-of-order.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (182 :: Int) bs)
        (cstm (Eec Nothing (Just CueParserTrackOutOfOrder)))
    it "rejects invalid ISRC values" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-invalid-isrc.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (161 :: Int) bs)
        (cstm (Eec (Just 3) (Just (CueParserInvalidTrackIsrc "123"))))
    it "rejects invalid number of seconds" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-invalid-seconds.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (168 :: Int) bs)
        (cstm (Eec (Just 1) (Just (CueParserInvalidSeconds 61))))
    it "rejects invalid number of frames" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-invalid-frames.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (171 :: Int) bs)
        (cstm (Eec (Just 1) (Just (CueParserInvalidFrames 77))))
    it "detects and reports indices that appear out of order" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-index-out-of-order.cue"
      parseCueSheet "" bs `shouldFailWith` err (posN (162 :: Int) bs)
        (cstm (Eec (Just 1) (Just CueParserTrackIndexOutOfOrder)))
    it "rejects duplicate CATALOG command" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-catalog.cue"
      let es = foldMap etoks
            [ "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (22 :: Int) bs)
        (utok 'C' <> es)
    it "rejects duplicate CDTEXTFILE command" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-cdtextfile.cue"
      let es = foldMap etoks
            [ "CATALOG"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (23 :: Int) bs)
        (utok 'C' <> es)
    it "rejects duplicate PERFORMER command" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-performer.cue"
      let es = foldMap etoks
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (30 :: Int) bs)
        (utok 'P' <> es)
    it "rejects duplicate TITLE command" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-title.cue"
      let es = foldMap etoks
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (17 :: Int) bs)
        (utok 'T' <> es)
    it "rejects duplicate SONGWRITER command" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-songwriter.cue"
      let es = foldMap etoks
            [ "CATALOG"
            , "CDTEXTFILE"
            , "FILE"
            , "PERFORMER"
            , "REM"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (18 :: Int) bs)
        (utok 'S' <> es)
    it "rejects duplicate FLAGS command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-flags.cue"
      let es = foldMap etoks
            [ "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (144 :: Int) bs)
        (utok 'F' <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate ISRC command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-isrc.cue"
      let es = foldMap etoks
            [ "FLAGS"
            , "INDEX"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (152 :: Int) bs)
        (utok 'I' <> utoks "IS" <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate PERFORMER command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-performer.cue"
      let es = foldMap etoks
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PREGAP"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (156 :: Int) bs)
        (utok 'P' <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate TITLE command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-title.cue"
      let es = foldMap etoks
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "SONGWRITER" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (146 :: Int) bs)
        (utok 'T' <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate SONGWRITER command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-songwriter.cue"
      let es = foldMap etoks
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "PREGAP"
            , "REM"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (150 :: Int) bs)
        (utok 'S' <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate PREGAP command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-pregap.cue"
      let es = foldMap etoks
            [ "FLAGS"
            , "INDEX"
            , "ISRC"
            , "PERFORMER"
            , "REM"
            , "SONGWRITER"
            , "TITLE" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (176 :: Int) bs)
        (utok 'P' <> es <> cstm (Eec (Just 1) Nothing))
    it "rejects duplicate POSTGAP command (in track)" $ do
      bs <- BL.readFile "cue-sheet-samples/parser-duplicate-track-postgap.cue"
      let es = foldMap etoks
            [ "FILE"
            , "REM"
            , "TRACK" ]
      parseCueSheet "" bs `shouldFailWith` err (posN (199 :: Int) bs)
        (utok 'P' <> es <> eeof)

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
