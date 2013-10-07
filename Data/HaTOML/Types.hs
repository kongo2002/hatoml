module Data.HaTOML.Types
    ( TOML(..)
    , TValue(..)
    , liftTOML
    , tinsert
    , tinsertWith
    , tempty
    ) where

import qualified Data.ByteString.Char8 as BS
import           Data.Map  ( Map, insert, insertWith, empty )
import           Data.Time ( UTCTime )

newtype TOML =
    TOML (Map BS.ByteString TValue)
    deriving ( Eq, Show, Ord )


data TValue = TString !BS.ByteString
            | TInteger !Integer
            | TDouble !Double
            | TBool !Bool
            | TArray ![TValue]
            | TDate !UTCTime
            | TGroup !TOML
              deriving ( Eq, Show, Ord )


liftTOML :: (Map BS.ByteString TValue -> Map BS.ByteString TValue) -> TOML -> TOML
liftTOML f (TOML m) = TOML $ f m


tinsert :: BS.ByteString -> TValue -> TOML -> TOML
tinsert k v m = liftTOML (insert k v) m


tinsertWith :: (TValue -> TValue -> TValue) -> BS.ByteString -> TValue -> TOML -> TOML
tinsertWith f k v m = liftTOML (insertWith f k v) m


tempty :: TOML
tempty = TOML empty
