module Data.HaTOML
    ( parse
    , encode
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.ByteString.Lazy.Builder

import Data.HaTOML.Encode
import Data.HaTOML.Parser
import Data.HaTOML.Types

parse :: BS.ByteString -> Maybe TOML
parse bs =
    case AB.parseOnly toml bs of
      Left _  -> Nothing
      Right r -> Just r

encode :: TOML -> LBS.ByteString
encode = toLazyByteString . fromToml
