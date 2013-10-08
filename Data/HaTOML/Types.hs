module Data.HaTOML.Types
    ( TOML(..)
    , TValue(..)
    , liftTOML
    , tinsert
    , tinsertWith
    , tempty
    ) where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Data.Time ( UTCTime )

newtype TOML =
    TOML (M.Map BS.ByteString TValue)
    deriving ( Eq, Show, Ord )


data TValue = TString !BS.ByteString
            | TInteger !Integer
            | TDouble !Double
            | TBool !Bool
            | TArray ![TValue]
            | TDate !UTCTime
            | TGroup !TOML
            | TTable ![TOML]
              deriving ( Eq, Show, Ord )


liftTOML :: (M.Map BS.ByteString TValue -> M.Map BS.ByteString TValue) -> TOML -> TOML
liftTOML f (TOML m) = TOML $ f m


tinsert :: BS.ByteString -> TValue -> TOML -> TOML
tinsert k v m = liftTOML (M.insert k v) m


tinsertWith :: (TValue -> TValue -> TValue) -> BS.ByteString -> TValue -> TOML -> TOML
tinsertWith f k v m = liftTOML (M.insertWith f k v) m


tempty :: TOML
tempty = TOML M.empty
