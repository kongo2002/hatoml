{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Parser where

import           Control.Applicative
import qualified Data.ByteString.Char8 as BS
import           Data.Attoparsec.ByteString.Char8
import           Data.Attoparsec.Combinator

import           Data.HaTOML.Types

toml = skipSpace *> many keyvalue <* endOfInput

keyvalue = do
    k <- keyname
    v <- equal *> value
    skipSpace
    return (k, v)

keyname :: Parser BS.ByteString
keyname = do
    skipSpace
    takeWhile1 $ notInClass " \t\n="

array :: Parser TValue
array = TArray <$> arrayValues value

arrayValues :: Parser TValue -> Parser [TValue]
arrayValues val = do
    skipSpace
    values <- ((val <* skipSpace ) `sepBy` (char ',' *> skipSpace) <* char ']')
    return values

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

skipEmpty = takeWhile1 $ notInClass " \t\n"
equal = skipSpace *> char '=' <* skipSpace
doubleQuote = char '"'
