module Data.HaTOML
    ( parse
    , parseTokens
    , encode
    ) where

import qualified Data.Attoparsec.ByteString.Char8 as AB
import qualified Data.ByteString.Char8 as BS
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.ByteString.Lazy.Builder
import           Data.List ( foldl' )
import qualified Data.Map as M

import Data.HaTOML.Encode
import Data.HaTOML.Parser
import Data.HaTOML.Types


-- | Try to parse the given bytestring into the matching
-- TOML representation.
parse :: BS.ByteString -> Either String TOML
parse bs = AB.parseOnly toml bs >>= parse'


parse' :: [TOMLToken] -> Either String TOML
parse' ts =
    return $ snd $ foldl' func ([], tempty) ts
  where
    func (_, t) (Left ks)      = (ks, insertGroup ks t)
    func (l, t) (Right (k, v)) = (l, insertValue (l++[k]) v t)


-- | Try to parse the given bytestring into a list of
-- matching TOML tokens.
parseTokens :: BS.ByteString -> Either String [TOMLToken]
parseTokens = AB.parseOnly toml


insertValue :: [BS.ByteString] -> TValue -> TOML -> TOML
insertValue [k] v m    = tinsert k v m
insertValue (k:ks) v m =
    tinsertWith func k v m
  where
    func _   (TGroup o) = TGroup $ insertValue ks v o
    -- TODO: according to the TOML spec this should
    -- throw an error
    func new _          = new


insertGroup :: [BS.ByteString] -> TOML -> TOML
insertGroup [k] m    = tinsert k (TGroup tempty) m
insertGroup (k:ks) m =
    tinsertWith func k (groupChain ks) m
  where
    func _ (TGroup o) = TGroup $ insertGroup ks o
    -- TODO: according to the TOML spec this should
    -- throw an error
    func _ v          = v


groupChain :: [BS.ByteString] -> TValue
groupChain []     = TGroup tempty
groupChain (k:ks) = TGroup $ TOML $ M.singleton k (groupChain ks)


-- | Encode the specified TOML data structure into
-- a lazy bytestring representation.
encode :: TOML -> LBS.ByteString
encode = toLazyByteString . fromToml
