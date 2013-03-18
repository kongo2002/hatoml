module Data.HaTOML.Types where

import qualified Data.ByteString.Char8 as BS
import           Data.Map  ( Map )
import           Data.Time ( UTCTime )

type TOML = Map BS.ByteString TValue

data TValue = TString !BS.ByteString
            | TInteger !Integer
            | TDouble !Double
            | TBool !Bool
            | TArray ![TValue]
            | TDate !UTCTime
              deriving ( Eq, Show, Ord )
