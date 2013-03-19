{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Data.Maybe     ( fromJust )

import           Test.HUnit     ( (@=?) )
import           Test.Framework ( defaultMain, testGroup, Test )
import           Test.Framework.Providers.HUnit

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
    testGroup "Parsing of examplary documents" [
        testCase "integer"          (toplevel (TInteger 4) @=? p "test = 4"),
        testCase "negative integer" (toplevel (TInteger (-201)) @=? p "test = -201"),
        testCase "double"           (toplevel (TDouble 34.2) @=? p "test = 34.2"),
        testCase "negative double"  (toplevel (TDouble (-0.01)) @=? p "test = -0.01")
        ]
    ]
