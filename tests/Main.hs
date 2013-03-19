{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map              as M
import           Data.Maybe                     ( fromJust )

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


tests :: [Test]
tests = [
    testGroup "Parsing of basic data types" [
        testCase "integer"          (toplevel (TInteger 4) @=? p "test = 4"),
        testCase "negative integer" (toplevel (TInteger (-201)) @=? p "test = -201"),
        testCase "double"           (toplevel (TDouble 34.2) @=? p "test = 34.2"),
        testCase "negative double"  (toplevel (TDouble (-0.01)) @=? p "test = -0.01"),
        testCase "boolean true"     (toplevel (TBool True) @=? p "test = true"),
        testCase "boolean false"    (toplevel (TBool False) @=? p "test = false"),
        testCase "strings"          (toplevel (TString "foobar") @=? p "test = \"foobar\"")
        ],
    testGroup "Parsing of arrays" [
        testCase "integer arrays"   (toplevel (TArray [TInteger 1, TInteger 2]) @=? p "test = [1,2]"),
        testCase "double arrays"    (toplevel (TArray [TDouble 1.0, TDouble 2.1]) @=? p "test = [1.0,2.1]"),
        testCase "boolean arrays"   (toplevel (TArray [TBool False, TBool True]) @=? p "test = [false,true]"),
        testCase "string arrays"    (toplevel (TArray [TString "a", TString "b"]) @=? p "test = [\"a\",\"b\"]")
        ]
    ]
