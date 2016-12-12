-- |
-- Module      :  Text.CueSheet
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module allows to construct, read, and write CUE sheets. The data
-- types are defined in such a way that incorrect CUE sheets are impossible
-- to represent. See 'parseCueSheet' for parsing of plain text CUE sheet
-- files and 'renderCueSheet' for rendering.
--
-- If you need to embed\/extract a CUE sheet in a FLAC file, see the @flac@
-- packages <https://hackage.haskell.org/package/flac>.

module Text.CueSheet
  ( -- * Types
    CueSheet      (..)
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
  , CueSheetException (..)
    -- * Parsing
  , Eec (..)
  , CueParserFailure (..)
  , parseCueSheet
    -- * Rendering
  , renderCueSheet )
where

import Text.CueSheet.Parser
import Text.CueSheet.Render
import Text.CueSheet.Types
