{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Parser.Header (headerParser) where

import Data.Char (toLower, toUpper)
import Data.Void (Void)
import Data.Functor ((<&>))
import Control.Applicative ((<|>))
import Text.Megaparsec (Parsec, try, choice, takeWhileP)
import Network.Data.Request (Header(..))
import Text.Parser (ByteStringWithChars(..))
import Text.Megaparsec.Char (char, string)
import qualified Data.ByteString.Char8 as BC

type HeaderParser = Parsec Void ByteStringWithChars Header

caseInsensitiveChar :: Char -> Parsec Void ByteStringWithChars Char
caseInsensitiveChar c = try ((char . toLower) c) <|> (char . toUpper) c

caseInsensitiveString :: BC.ByteString -> Parsec Void ByteStringWithChars BC.ByteString
caseInsensitiveString st = case BC.uncons st of
  Just (x, xs) -> caseInsensitiveChar x >>= (\y -> BC.cons y <$> caseInsensitiveString xs)
  Nothing -> pure ""

userAgentHParser :: HeaderParser
userAgentHParser = caseInsensitiveString "User-Agent" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> UserAgentH

hostParser :: HeaderParser
hostParser = caseInsensitiveString "Host" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> HostH

acceptParser :: HeaderParser
acceptParser = caseInsensitiveString "Accept" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> (AcceptH . BC.unpack)

headerParser :: HeaderParser
headerParser = choice [try userAgentHParser, try acceptParser, hostParser] <* string "\r\n"