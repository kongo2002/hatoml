module Data.HaTOML
    ( encode
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.ByteString.Lazy.Builder
import qualified Data.Map as M

import Data.HaTOML.Encode
import Data.HaTOML.Parser
import Data.HaTOML.Types


{- parse :: BS.ByteString -> Either String TOML -}
{- parse = AB.parseOnly toml -}


{- parseMaybe :: BS.ByteString -> Maybe TOML -}
{- parseMaybe bs = -}
    {- case AB.parseOnly toml bs of -}
      {- Left _  -> Nothing -}
      {- Right r -> Just r -}


parseTokens :: BS.ByteString -> Either String [TOMLToken]
parseTokens = AB.parseOnly toml


parseTokensMaybe :: BS.ByteString -> Maybe [TOMLToken]
parseTokensMaybe bs =
    case AB.parseOnly toml bs of
      Left _  -> Nothing
      Right r -> Just r


encode :: TOML -> LBS.ByteString
encode = toLazyByteString . fromToml


insertValue :: [BS.ByteString] -> TValue -> TOML -> TOML
insertValue [k] v m    = tinsert k v m
insertValue (k:ks) v m =
    tinsertWith func k v m
  where
    func _   (TGroup o) = TGroup $ insertValue ks v o
    func new _          = new


insertGroup :: [BS.ByteString] -> TOML -> TOML
insertGroup [k] m    = tinsert k (TGroup tempty) m
insertGroup (k:ks) m =
    tinsertWith func k (groupChain ks) m
  where
    func _ (TGroup o) = TGroup $ insertGroup ks o
    func _ v          = v


groupChain :: [BS.ByteString] -> TValue
groupChain []     = TGroup tempty
groupChain (k:ks) = TGroup $ TOML $ M.singleton k (groupChain ks)


