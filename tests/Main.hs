{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Maybe                     ( fromJust )
import           Data.Time                      ( fromGregorian, UTCTime(..) )

import           Test.HUnit                     ( (@=?) )
import           Test.Framework                 ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit ( testCase )

import           Data.HaTOML
import           Data.HaTOML.Types


main :: IO ()
main = defaultMain tests


toplevel :: TValue -> TOML
toplevel val =
    TOML $ M.fromList [("test", val)]


p :: BS.ByteString -> TOML
p = fromJust . parse


testDate :: UTCTime
testDate =
    UTCTime day time
  where
    day  = fromGregorian 1990 9 20
    time = 60 * 60 * 14


tests :: [Test]
tests = [
    testGroup "Parsing of basic data types" [
        testCase "integer"          (toplevel (TInteger 4) @=? p "test = 4"),
        testCase "negative integer" (toplevel (TInteger (-201)) @=? p "test = -201"),
        testCase "double"           (toplevel (TDouble 34.2) @=? p "test = 34.2"),
        testCase "negative double"  (toplevel (TDouble (-0.01)) @=? p "test = -0.01"),
        testCase "boolean true"     (toplevel (TBool True) @=? p "test = true"),
        testCase "boolean false"    (toplevel (TBool False) @=? p "test = false"),
        testCase "string"           (toplevel (TString "foobar") @=? p "test = \"foobar\""),
        testCase "datetime"         (toplevel (TDate testDate) @=? p "test = 1990-09-20T14:00:00Z")
        ],
    testGroup "Parsing of arrays" [
        testCase "integer arrays" (toplevel (TArray [TInteger 1, TInteger 2]) @=? p "test = [1,2]"),
        testCase "double arrays"  (toplevel (TArray [TDouble 1.0, TDouble 2.1]) @=? p "test = [1.0,2.1]"),
        testCase "boolean arrays" (toplevel (TArray [TBool False, TBool True]) @=? p "test = [false,true]"),
        testCase "string arrays"  (toplevel (TArray [TString "a", TString "b"]) @=? p "test = [\"a\",\"b\"]")
        ],
    testGroup "Parsing of strings" [
        testCase "empty string"         (toplevel (TString "") @=? p "test = \"\""),
        testCase "special chars #1"     (toplevel (TString "] ") @=? p "test = \"] \""),
        testCase "special chars #2"     (toplevel (TString " # ") @=? p "test = \" # \""),
        testCase "special chars #3"     (toplevel (TString "#") @=? p "test = \"#\""),
        testCase "special chars #4"     (toplevel (TString " \n \t ") @=? p "test = \" \\n \\t \""),
        testCase "unicode chars"        (toplevel (TString " é ") @=? p "test = \" \\u00E9 \""),
        testCase "escaped double quote" (toplevel (TString " \"'s '\"") @=? p "test = \" \\\"'s '\\\"\""),
        testCase "escaped characters"   (toplevel (TString "\"\n\t\b") @=? p "test = \"\\\"\\n\\t\\b\"")
        ]
    ]
