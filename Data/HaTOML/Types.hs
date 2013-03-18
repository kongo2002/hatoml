module Data.HaTOML.Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Map  ( Map )
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
