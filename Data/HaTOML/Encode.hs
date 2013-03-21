{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Encode where

import           Data.Monoid                        ( mappend, Monoid )
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII ( integerDec, doubleDec )
import qualified Data.ByteString.Char8 as BS
import qualified Data.Map as M
import           Numeric                            ( showHex )

import Data.HaTOML.Types


fromToml :: TOML -> Builder
fromToml = fromValue . TGroup


fromValue :: TValue -> Builder
fromValue (TBool b) =
    stringUtf8 enc
  where enc = if b then "true" else "false"
fromValue (TInteger i) = integerDec i
fromValue (TDouble d) = doubleDec d
fromValue (TString s) =
    charUtf8 '"' <> quote s <> charUtf8 '"'
  where
    quote str =
        case BS.uncons t of
          Nothing      -> byteString h
          Just (c, t') -> byteString h <> stringUtf8 (escape c) <> quote t'
      where
        (h, t) = BS.break isEscape str
    isEscape c = c == '\"' ||
                 c == '\\' ||
                 c < '\x20'
    escape '\"' = "\\\""
    escape '\\' = "\\\\"
    escape '\n' = "\\n"
    escape '\r' = "\\r"
    escape '\t' = "\\t"
    escape c
        | c < '\x20' = "\\u" ++ replicate (4 - length hex) '0' ++ hex
        | otherwise  = [c]
        where hex = showHex (fromEnum c) ""
fromValue (TGroup (TOML m)) =
    M.foldrWithKey' proc (charUtf8 '\n') m
  where
    proc k v a = charUtf8 '\n' <> val k v <> a
    val k v = byteString k <> charUtf8 '=' <> fromValue v


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
