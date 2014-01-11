{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Encode
    ( fromToml
    , fromValue
    ) where

import           Data.Monoid                        ( mappend, mconcat, Monoid )
import           Data.ByteString.Lazy.Builder
import           Data.ByteString.Lazy.Builder.ASCII ( integerDec, doubleDec )
import qualified Data.ByteString.Char8 as BS
import           Data.List                          ( intersperse )
import qualified Data.Map as M
import           Data.Time.Format                   ( formatTime )

import           Numeric                            ( showHex )

import           System.Locale                      ( defaultTimeLocale, iso8601DateFormat )

import Data.HaTOML.Types


fromToml :: TOML -> Builder
fromToml = fromValue [] . TGroup


fromValue :: [BS.ByteString] -> TValue -> Builder
fromValue _ (TBool b) = stringUtf8 enc
  where enc = if b then "true" else "false"

fromValue _ (TInteger i) = integerDec i

fromValue _ (TDouble d) = doubleDec d

fromValue _ (TString s) =
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

fromValue _ (TDate d) =
    stringUtf8 date <> charUtf8 'Z'
  where
    format = iso8601DateFormat $ Just "%X"
    date   = formatTime defaultTimeLocale format d

fromValue sec (TGroup (TOML m)) =
    M.foldrWithKey' proc empty m
  where
    empty = byteString BS.empty
    nl = charUtf8 '\n'
    proc k v a = nl <> value k v <> a

    value k v =
        case v of
          g@(TGroup _) -> group <> fromValue sec' g
          _            -> byteString k <> stringUtf8 " = " <> fromValue sec' v
      where
        sec'    = sec ++ [k]
        group   = charUtf8 '[' <> section sec' <> charUtf8 ']'
        section = mconcat . intersperse (charUtf8 '.') . map byteString

fromValue _ (TArray []) = stringUtf8 "[]"
fromValue s (TArray a) =
    charUtf8 '[' <>
    fromValue s (head a) <>
    foldr proc (charUtf8 ']') (tail a)
  where
    proc x a = stringUtf8 ", " <> fromValue s x <> a


infixr 4 <>
(<>) :: Monoid m => m -> m -> m
(<>) = mappend
{-# INLINE (<>) #-}
