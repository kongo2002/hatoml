{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Time                      ( fromGregorian, UTCTime(..) )

import           Test.HUnit                     ( (@=?) )
import           Test.Framework                 ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit ( testCase )

import           Data.HaTOML
import           Data.HaTOML.Types


main :: IO ()
main = defaultMain tests


val :: TValue -> TOML
val v =
    TOML $ M.fromList [("test", v)]


kv :: BS.ByteString -> TValue -> TOML
kv k v =
    TOML $ M.fromList [(k, v)]


grp :: BS.ByteString -> BS.ByteString -> TValue -> TOML
grp g k v =
    kv g $ TGroup $ TOML $ M.fromList [(k, v)]


p :: BS.ByteString -> TOML
p bs = let (Right toml) = parse bs
       in toml


testDate :: UTCTime
testDate =
    UTCTime day time
  where
    day  = fromGregorian 1990 9 20
    time = 60 * 60 * 14


tests :: [Test]
tests = [
  testGroup "Parsing of basic data types" [
    testCase "integer"          (val (TInteger 4) @=? p "test = 4"),
    testCase "negative integer" (val (TInteger (-201)) @=? p "test = -201"),
    testCase "double"           (val (TDouble 34.2) @=? p "test = 34.2"),
    testCase "negative double"  (val (TDouble (-0.01)) @=? p "test = -0.01"),
    testCase "boolean true"     (val (TBool True) @=? p "test = true"),
    testCase "boolean false"    (val (TBool False) @=? p "test = false"),
    testCase "string"           (val (TString "foobar") @=? p "test = \"foobar\""),
    testCase "datetime"         (val (TDate testDate) @=? p "test = 1990-09-20T14:00:00Z")
    ],
  testGroup "Parsing of arrays" [
    testCase "integer arrays"         (val (TArray [TInteger 1, TInteger 2]) @=? p "test = [1,2]"),
    testCase "double arrays"          (val (TArray [TDouble 1.0, TDouble 2.1]) @=? p "test = [1.0,2.1]"),
    testCase "boolean arrays"         (val (TArray [TBool False, TBool True]) @=? p "test = [false,true]"),
    testCase "string arrays"          (val (TArray [TString "a", TString "b"]) @=? p "test = [\"a\",\"b\"]"),
    testCase "array of array"         (val (TArray [TArray [TInteger 1]]) @=? p "test = [[1]]"),
    testCase "array of arrays"        (val (TArray [TArray [TInteger 1], TArray [TInteger 2]]) @=? p "test = [[1],[2]]"),
    testCase "mixed array"            (val (TArray [TArray [TInteger 1], TArray [TBool False]]) @=? p "test = [[1], [false]]"),
    testCase "multiline array"        (val (TArray [TInteger 1, TInteger 2]) @=? p "test = [\n1,\n2\n]"),
    testCase "array (trailing comma)" (val (TArray [TInteger 1, TInteger 2]) @=? p "test = [1,2,]")
    ],
  testGroup "Parsing of strings" [
    testCase "empty string"         (val (TString "") @=? p "test = \"\""),
    testCase "special chars #1"     (val (TString "] ") @=? p "test = \"] \""),
    testCase "special chars #2"     (val (TString " # ") @=? p "test = \" # \""),
    testCase "special chars #3"     (val (TString "#") @=? p "test = \"#\""),
    testCase "special chars #4"     (val (TString " \n \t ") @=? p "test = \" \\n \\t \""),
    testCase "unicode chars"        (val (TString " é ") @=? p "test = \" \\u00E9 \""),
    testCase "escaped double quote" (val (TString " \"'s '\"") @=? p "test = \" \\\"'s '\\\"\""),
    testCase "escaped characters"   (val (TString "\"\n\t\b") @=? p "test = \"\\\"\\n\\t\\b\"")
    ],
  testGroup "Parsing of comments" [
    testCase "comment at start"    (val (TInteger 3) @=? p "# comment\n test = 3\n"),
    testCase "comment after"       (val (TInteger 4) @=? p "test = 4 # comment"),
    testCase "comment at end"      (val (TInteger 5) @=? p "test = 5\n# comment"),
    testCase "comment in array #1" (val (TArray [TInteger 5, TInteger 28]) @=? p "test = [5,28\n# comment\n]"),
    testCase "comment in array #2" (val (TArray [TInteger 5, TInteger 28]) @=? p "test = [5,28# comment\n]"),
    testCase "comment in array #3" (val (TArray [TInteger 5, TInteger 28]) @=? p "test = [5 # comment\n,28,\n# comment\n]"),
    testCase "comment in array #4" (val (TArray [TInteger 5, TInteger 28]) @=? p "test = [\n# comment\n5,28# comment\n]")
    ],
  testGroup "Parsing of groups" [
    testCase "groups #1"    (grp "x" "y" (TInteger 22) @=? p "[x]\ny = 22"),
    testCase "groups #2"    (grp "x#" "y" (TInteger 11) @=? p "[x#]\ny = 11"),
    testCase "key names #1" (kv "test?" (TInteger 5) @=? p "test? = 5"),
    testCase "key names #2" (kv "test_1" (TInteger 9) @=? p "test_1=9")
    ],
  testGroup "Various tests" [
    testCase "empty result"    ((TOML $ M.empty) @=? p ""),
    testCase "whitespace only" ((TOML $ M.empty) @=? p "    \n   \t  ")
    ]
  ]
