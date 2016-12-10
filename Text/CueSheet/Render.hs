-- |
-- Module      :  Text.CueSheet.Render
-- Copyright   :  © 2016 Mark Karpov
-- License     :  BSD 3 clause
--
-- Maintainer  :  Mark Karpov <markkarpov@openmailbox.org>
-- Stability   :  experimental
-- Portability :  portable
--
-- The module contains just CUE sheet render. You probably want to import
-- "Text.CueSheet" instead.

module Text.CueSheet.Render
  ( renderCueSheet )
where

import Text.CueSheet.Types
import qualified Data.ByteString.Lazy as BL

-- | Render a CUE sheet to a lazy 'BL.ByteString'.

renderCueSheet :: CueSheet -> BL.ByteString
renderCueSheet = undefined -- TODO
