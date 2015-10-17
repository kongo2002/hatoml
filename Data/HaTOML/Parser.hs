{-# LANGUAGE OverloadedStrings #-}

module Data.HaTOML.Parser
    ( toml
    , TOMLToken(..)
    ) where


import           Prelude hiding   ( takeWhile )
import           Control.Applicative

import           Data.Attoparsec.ByteString.Char8
import qualified Data.ByteString.Char8 as BS
import           Data.Time.Format ( parseTimeM, defaultTimeLocale
                                  , iso8601DateFormat )
import           Data.Scientific  ( floatingOrInteger )

import           Numeric          ( readHex )

import           Data.HaTOML.Types


data TOMLToken = TKeyGroup [BS.ByteString]
               | TTableArray [BS.ByteString]
               | TKeyValue (BS.ByteString, TValue)
                 deriving ( Eq, Show, Ord )


toml :: Parser [TOMLToken]
toml = skip *> many element <* endOfInput


element :: Parser TOMLToken
element =
    skip *> (table <|> group <|> keyvalue)


table :: Parser TOMLToken
table = do
    _ <- char '['
    _ <- char '['
    TTableArray <$> (part `sepBy1` char '.') <* char ']' <* char ']'
  where
    part = takeWhile1 $ notInClass "]."


group :: Parser TOMLToken
group = do
    _ <- char '['
    TKeyGroup <$> (part `sepBy1` char '.') <* char ']'
  where
    part = takeWhile1 $ notInClass "]."


keyvalue :: Parser TOMLToken
keyvalue = do
    k <- keyname
    v <- equal *> value
    skip
    return $ TKeyValue (k, v)


keyname :: Parser BS.ByteString
keyname = takeWhile1 $ notInClass " \t\n="


array :: Parser TValue
array = TArray <$> arrayValues value


arrayValues :: Parser TValue -> Parser [TValue]
arrayValues val =
    skip *> ((val <* skip) `sepBy` (char ',' *> skip) <* arrayEnd)


arrayEnd = do
    skip
    optional $ char ','
    skip
    char ']'


tstring = many' tchar <* doubleQuote


tchar = char '\\' *> (tescape <|> tunicode) <|> satisfy (`BS.notElem` "\"\\")


tescape = choice (zipWith decode "bnfrt\\\"/" "\b\n\f\r\t\\\"/")
    where decode c r = r <$ char c


tunicode = char 'u' *> (decode <$> count 4 hexDigit)
    where
        isHexDigit c = isDigit c ||
                       (c >= 'a' && c <= 'f') ||
                       (c >= 'A' && c <= 'F')
        hexDigit = satisfy isHexDigit
        decode x = toEnum code
            where ((code,_):_) = readHex x


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
    n <- floatingOrInteger <$> scientific
    case n of
      (Left f)  -> return $ TDouble f
      (Right i) -> return $ TInteger i


skip :: Parser ()
skip = do
    _ <- scan False $ \s c ->
        case (s, c) of
          (True, '\r')  -> Just False
          (True, '\n')  -> Just False
          (True, _)     -> Just True
          (_, '#')      -> Just True
          _ | isSpace c -> Just s
          _     -> Nothing
    return ()


date :: Parser TValue
date = do
    d <- takeTill (== 'Z') <* char 'Z'
    parseDate (BS.unpack d)
  where
    iso8601      = iso8601DateFormat $ Just "%X"
    parseDate bs =
      case parseTimeM True defaultTimeLocale iso8601 bs of
        Just t  -> return $ TDate t
        Nothing -> fail "failed to parse date"


eol c = c == '\n' || c == '\r'
equal = skipSpace *> char '=' <* skipSpace
doubleQuote = char '"'
