{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Parser where

import           Prelude hiding   ( takeWhile )
import           Control.Applicative

import qualified Data.Map as M
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Format ( parseTime )
import           System.Locale    ( defaultTimeLocale, iso8601DateFormat )

import           Data.HaTOML.Types

toml :: Parser TOML
toml = (TOML . M.fromList) <$> values
  where
    values = skip *> many keyvalue <* endOfInput

keyvalue :: Parser (BS.ByteString, TValue)
keyvalue = do
    k <- keyname
    v <- equal *> value
    skip
    return (k, v)

keyname :: Parser BS.ByteString
keyname = do
    skipSpace
    takeWhile1 $ notInClass " \t\n="

array :: Parser TValue
array = TArray <$> arrayValues value

arrayValues :: Parser TValue -> Parser [TValue]
arrayValues val =
    skipSpace *> ((val <* skipSpace ) `sepBy` (char ',' *> skipSpace) <* char ']')

tstring = many' tchar <* doubleQuote

tchar = char '\\' *> (tescape {- <|> TODO: unicode -}) <|> satisfy (`BS.notElem` "\"\\")

tescape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c

value :: Parser TValue
value =
    others <|>
    date <|>
    num
  where
    others = do
      c <- satisfy (`BS.elem` "[\"tf")
      case c of
        '[' -> array
        '"' -> (TString . BS.pack) <$> tstring
        't' -> string "rue" *> pure (TBool True)
        'f' -> string "alse" *> pure (TBool False)
        _   -> error "captain! we've been hit!"

num :: Parser TValue
num = do
    n <- number
    case n of
      I n -> return $ TInteger n
      D n -> return $ TDouble n

skip :: Parser ()
skip = do
    _ <- scan False $ \s c ->
        case (s, c) of
          (True, '\r')  -> Just False
          (True, '\n')  -> Just False
          (True, _)     -> Just True
          (_, '#')      -> Just True
          _ | isSpace c -> Just s
          otherwise     -> Nothing
    return ()

date :: Parser TValue
date = do
    d <- takeTill (== 'Z') <* char 'Z'
    parseDate (BS.unpack d)
  where
    iso8601      = iso8601DateFormat $ Just "%X"
    parseDate bs =
      case parseTime defaultTimeLocale iso8601 bs of
        Just t  -> return $ TDate t
        Nothing -> fail "failed to parse date"


eol c = c == '\n' || c == '\r'
equal = skipSpace *> char '=' <* skipSpace
doubleQuote = char '"'
