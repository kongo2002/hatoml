{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Parser where

import           Control.Applicative

import qualified Data.Map as M
import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS

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

tstring :: Parser BS.ByteString
tstring = takeWhile1 (/= '"') <* doubleQuote

value :: Parser TValue
value = others <|> num
  where
    others = do
      c <- satisfy (`BS.elem` "[\"tf")
      case c of
        '[' -> array
        '"' -> TString <$> tstring
        't' -> string "rue" *> pure (TBool True)
        'f' -> string "alse" *> pure (TBool False)
        _   -> error "captain! we've been hit!"

num :: Parser TValue
num = do
    n <- number
    case n of
      I n -> return $ TInteger n
      D n -> return $ TDouble n

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

eol c = c == '\n' || c == '\r'
equal = skipSpace *> char '=' <* skipSpace
doubleQuote = char '"'
