module Data.HaTOML
    ( parse
    , parseGroups
    , encode
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.ByteString.Lazy.Builder

import Data.HaTOML.Encode
import Data.HaTOML.Parser
import Data.HaTOML.Types


parse :: BS.ByteString -> Either String TOML
parse = AB.parseOnly toml


parseMaybe :: BS.ByteString -> Maybe TOML
parseMaybe bs =
    case AB.parseOnly toml bs of
      Left _  -> Nothing
      Right r -> Just r


parseGroups :: BS.ByteString -> Either String [Either [BS.ByteString] (BS.ByteString, TValue)]
parseGroups = AB.parseOnly tomlGroups


parseGroupsMaybe :: BS.ByteString -> Maybe [Either [BS.ByteString] (BS.ByteString, TValue)]
parseGroupsMaybe bs =
    case AB.parseOnly tomlGroups bs of
      Left _  -> Nothing
      Right r -> Just r


encode :: TOML -> LBS.ByteString
encode = toLazyByteString . fromToml
