module Data.HaTOML where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BS

import Data.HaTOML.Parser
import Data.HaTOML.Types

parse :: BS.ByteString -> Maybe TOML
parse bs =
    case AB.parseOnly toml bs of
      Left _  -> Nothing
      Right r -> Just r
