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

module Text.CueSheet.Parser
  ( parseCueSheet )
where

import Text.CueSheet.Types
import Text.Megaparsec
import qualified Data.ByteString.Lazy as BL

-- | Parse a CUE sheet from a lazy 'BL.ByteString'.

parseCueSheet :: BL.ByteString -> Either (ParseError Dec Char) CueSheet
parseCueSheet = undefined -- TODO
