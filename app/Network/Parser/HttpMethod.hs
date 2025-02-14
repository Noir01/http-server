{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Network.Parser.HttpMethod (methodParser) where
import Network.HTTP.Types (StdMethod (..))
import Data.Void (Void)
import Text.Megaparsec (Parsec, choice)
import Text.Megaparsec.Char (string)
import Data.Functor (($>))
import Text.Parser (ByteStringWithChars)

type MethodParser = Parsec Void ByteStringWithChars StdMethod

getParser :: MethodParser
getParser = string "GET" $> GET

postParser :: MethodParser
postParser = string "POST" $> POST

patchParser :: MethodParser
patchParser = string "PATCH" $> PATCH

putParser :: MethodParser
putParser = string "PUT" $> PUT

optionsParser :: MethodParser
optionsParser = string "OPTIONS" $> OPTIONS

deleteParser :: MethodParser
deleteParser = string "DELETE" $> DELETE

connectParser :: MethodParser
connectParser = string "CONNECT" $> CONNECT

traceParser :: MethodParser
traceParser = string "TRACE" $> TRACE

headParser :: MethodParser
headParser = string "HEAD" $> HEAD

methodParser :: MethodParser
methodParser = choice [getParser, postParser, patchParser, putParser, optionsParser, deleteParser, connectParser, traceParser, headParser]
