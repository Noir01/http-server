{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Parser.Request (requestParser) where

import Network.Data.Request (Req(..))
import Network.Parser.HttpVersion (httpVersionParser)
import Network.Parser.HttpMethod (methodParser)
import Text.Megaparsec (Parsec, choice, try)
import Data.Void (Void)
import Text.Parser (ByteStringWithChars)
import Text.Megaparsec.Char (char, string)
import Network.Parser.URI (homeParser, userAgentParser, echoParser, fileParser, unknownParser)

type ReqParser = Parsec Void ByteStringWithChars Req

requestParser :: ReqParser
requestParser = do
  m <- methodParser
  _ <- char ' '
  u <- choice [try homeParser, try userAgentParser, try echoParser, try fileParser, unknownParser]
  _ <- char ' '
  v <- httpVersionParser
  _ <- string "\r\n"
  return $ Req {method = m, uri = u, httpVersion = v}