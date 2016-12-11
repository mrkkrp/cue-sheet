--
-- Render tests for the ‘cue-sheet’ package.
--
-- Copyright © 2016 Mark Karpov <markkarpov@openmailbox.org>
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

module Text.CueSheet.RenderSpec
  ( spec )
where

import Control.Monad.Catch
import Data.List.NonEmpty (NonEmpty (..))
import Test.Hspec
import Test.QuickCheck
import Text.CueSheet.Render
import Text.CueSheet.Types
import qualified Data.ByteString.Lazy as BL
import qualified Data.List.NonEmpty   as NE

spec :: Spec
spec =
  describe "renderCueSheet" $ do
    it "doesn't ever produce trailing whitespace" $
      property $ \csrf cueSheet -> do
        let r = renderCueSheet csrf cueSheet
            f x = if csrf then BL.take (BL.length x - 1) x else x
        BL.split 10 r `shouldNotSatisfy`
          any ((" " `BL.isSuffixOf`) . f)
    it "always ends with the specified new line sequence" $
      property $ \csrf cueSheet -> do
        let r   = renderCueSheet csrf cueSheet
            eol = if csrf then "\r\n" else "\n"
        r `shouldSatisfy` (eol `BL.isSuffixOf`)
    it "doesn't look too bad" $ do
      expected <- BL.readFile "cue-sheet-samples/rendered0.cue"
      cueSheet <- testCueSheet
      BL.writeFile "cue-sheet-samples/actual.cue" (renderCueSheet False cueSheet)
      renderCueSheet False cueSheet `shouldBe` expected

-- | A manually constructed testing CUE sheet.

testCueSheet :: MonadThrow m => m CueSheet
testCueSheet = do
  mcn        <- mkMcn "1112223334445"
  performer  <- mkCueText "The Famous Foobar"
  title      <- mkCueText "Bobla Fett"
  songwriter <- mkCueText "Paul"
  isrc       <- mkIsrc    "agree1234567"
  pregap     <- fromMmSsFf 1 0 13
  index      <- fromMmSsFf 0 0 15
  postgap    <- fromMmSsFf 0 8 9
  return CueSheet
    { cueCatalog    = Just mcn
    , cueCdTextFile = Just "/home/mark/mycdtextfile.txt"
    , cuePerformer  = Just performer
    , cueTitle      = Just title
    , cueSongwriter = Just songwriter
    , cueFirstTrackNumber = 1
    , cueFiles      = NE.fromList
      [ CueFile
        { cueFileName   = "the-famous-foobar-cd1.flac"
        , cueFileType   = Wave
        , cueFileTracks = NE.fromList
          [ CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = True
            , cueTrackSerialCopyManagement = True
            , cueTrackType                 = CueTrackAudio
            , cueTrackIsrc                 = Just isrc
            , cueTrackTitle                = Just title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = index :| []
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = True
            , cueTrackFourChannelAudio     = True
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackCdg
            , cueTrackIsrc                 = Just isrc
            , cueTrackTitle                = Just title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Just pregap
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = index :| []
            , cueTrackPostgap              = Just postgap } ] }
      , CueFile
        { cueFileName   = "the-famous-foobar-cd2.mp3"
        , cueFileType   = MP3
        , cueFileTracks = NE.fromList
          [ CueTrack
            { cueTrackDigitalCopyPermitted = False
            , cueTrackFourChannelAudio     = False
            , cueTrackPreemphasisEnabled   = False
            , cueTrackSerialCopyManagement = False
            , cueTrackType                 = CueTrackMode1_2048
            , cueTrackIsrc                 = Just isrc
            , cueTrackTitle                = Just title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = index :| []
            , cueTrackPostgap              = Nothing }
          , CueTrack
            { cueTrackDigitalCopyPermitted = True
            , cueTrackFourChannelAudio     = True
            , cueTrackPreemphasisEnabled   = True
            , cueTrackSerialCopyManagement = True
            , cueTrackType                 = CueTrackMode2_2336
            , cueTrackIsrc                 = Just isrc
            , cueTrackTitle                = Just title
            , cueTrackPerformer            = Just performer
            , cueTrackSongwriter           = Nothing
            , cueTrackPregap               = Nothing
            , cueTrackPregapIndex          = Nothing
            , cueTrackIndices              = index :| []
            , cueTrackPostgap              = Nothing } ] }
      ]
    }
