{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Parser.URI (unknownParser, echoParser, fileParser, homeParser, userAgentParser) where

import Network.Data.Request (URI(..))
import Text.Megaparsec (Parsec, failure, takeWhileP)
import Data.Void (Void)
import Control.Applicative (optional)
import Text.Parser (ByteStringWithChars)
import Text.Megaparsec.Char (char, string)
import qualified Data.Set as Set


type URIParser = Parsec Void ByteStringWithChars URI

unknownParser :: URIParser
unknownParser = Unknown <$> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', ' '])

echoParser :: URIParser
echoParser = string "/echo" *> optional (char '/') *> (Echo <$> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']))

homeParser :: URIParser
homeParser = char '/' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']) >>= (\consumed -> if consumed == "" then pure Home else failure Nothing Set.empty)

userAgentParser :: URIParser
userAgentParser = char '/' *> string "user-agent" *> optional (char '/') *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']) >>= (\consumed -> if consumed == "" then pure UserAgent else failure Nothing Set.empty)

fileParser :: URIParser
fileParser = string "/files" *> optional (char '/') *> (File <$> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']))