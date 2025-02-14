{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser (requestParser, ByteStringWithChars (..), headerParser) where

import Control.Applicative (optional, (<|>))
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BC
import Data.Char (isDigit, toLower, toUpper)
import Data.Functor (($>), (<&>))
import qualified Data.List.NonEmpty as NE (toList)
import Data.Proxy (Proxy (..))
import qualified Data.Set as Set
import Data.Void (Void)
import Network.HTTP.Types (HttpVersion (..), StdMethod (..))
import Request (Header (..), Req (..), URI (..))
import Text.Megaparsec (Parsec, choice, failure, many, manyTill, satisfy, takeWhileP, try)
import Text.Megaparsec.Char (char, string)
import Text.Megaparsec.Debug (dbg')
import Text.Megaparsec.Stream (ShareInput (..), Stream (..), Token, Tokens, VisualStream (..))

newtype ByteStringWithChars = BS BC.ByteString

instance Stream (ShareInput ByteStringWithChars) where
  type Token (ShareInput ByteStringWithChars) = Char
  type Tokens (ShareInput ByteStringWithChars) = BC.ByteString
  tokenToChunk Proxy = BC.singleton
  tokensToChunk Proxy = BC.pack
  chunkToTokens Proxy = BC.unpack
  chunkLength Proxy = BC.length
  chunkEmpty Proxy = BC.null
  take1_ (ShareInput (BS s)) = second (ShareInput . BS) <$> BC.uncons s
  takeN_ n (ShareInput (BS s))
    | n <= 0 = Just (BC.empty, ShareInput (BS s))
    | BC.null s = Nothing
    | otherwise = Just . second (ShareInput . BS) $ BC.splitAt n s
  takeWhile_ p (ShareInput (BS s)) = second (ShareInput . BS) $ BC.span p s

instance Stream ByteStringWithChars where
  type Token ByteStringWithChars = Token (ShareInput ByteStringWithChars)
  type Tokens ByteStringWithChars = Tokens (ShareInput ByteStringWithChars)
  tokenToChunk Proxy = tokenToChunk (Proxy :: Proxy (ShareInput ByteStringWithChars))
  tokensToChunk Proxy = tokensToChunk (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkToTokens Proxy = chunkToTokens (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkLength Proxy = chunkLength (Proxy :: Proxy (ShareInput ByteStringWithChars))
  chunkEmpty Proxy = chunkEmpty (Proxy :: Proxy (ShareInput ByteStringWithChars))
  take1_ s = second unShareInput <$> take1_ (ShareInput s)
  takeN_ n s = second unShareInput <$> takeN_ n (ShareInput s)
  takeWhile_ p s = second unShareInput $ takeWhile_ p (ShareInput s)

instance VisualStream ByteStringWithChars where
  showTokens Proxy = NE.toList

type MethodParser = Parsec Void ByteStringWithChars StdMethod

type HTTPVersionParser = Parsec Void ByteStringWithChars HttpVersion

type URIParser = Parsec Void ByteStringWithChars URI

type HeaderParser = Parsec Void ByteStringWithChars Header

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

httpVersionParser :: HTTPVersionParser
httpVersionParser = string "HTTP/" *> many (satisfy isDigit) >>= (\major -> char '.' *> many (satisfy isDigit) >>= (\minor -> pure $ HttpVersion (maybe (error "Unexpected") fst . BC.readInt $ BC.pack major) (maybe (error "Unexpected") fst . BC.readInt $ BC.pack minor)))

unknownParser :: URIParser
unknownParser = Unknown <$> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', ' '])

type ReqParser = Parsec Void ByteStringWithChars Req

requestParser :: ReqParser
requestParser = do
  m <- methodParser
  _ <- char ' '
  u <- choice [try homeParser, try userAgentParser, try echoParser, unknownParser]
  _ <- char ' '
  v <- httpVersionParser
  _ <- string "\r\n"
  return $ Req {method = m, uri = u, httpVersion = v}

echoParser :: URIParser
echoParser = (optional (char '/') *> string "echo/") *> (Echo <$> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']))

homeParser :: URIParser
homeParser = char '/' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']) >>= (\consumed -> if consumed == "" then pure Home else failure Nothing Set.empty)

userAgentParser :: URIParser
userAgentParser = char '/' *> string "user-agent/" *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n', '\\', ' ']) >>= (\consumed -> if consumed == "" then pure UserAgent else failure Nothing Set.empty)

caseInsensitiveChar :: Char -> Parsec Void ByteStringWithChars Char
caseInsensitiveChar c = try ((char . toLower) c) <|> (char . toUpper) c

caseInsensitiveString :: BC.ByteString -> Parsec Void ByteStringWithChars BC.ByteString
caseInsensitiveString st = case BC.uncons st of
  Just (x, xs) -> caseInsensitiveChar x >>= (\y -> BC.cons y <$> caseInsensitiveString xs)
  Nothing -> pure ""

userAgentHParser :: HeaderParser
userAgentHParser = dbg' "header user agent" $ caseInsensitiveString "User-Agent" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> UserAgentH

hostParser :: HeaderParser
hostParser = dbg' "header host" $ caseInsensitiveString "Host" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> HostH

acceptParser :: HeaderParser
acceptParser = dbg' "header accept" $ caseInsensitiveString "Accept" *> char ':' *> char ' ' *> takeWhileP Nothing (\tok -> tok `notElem` ['\r', '\n']) <&> (AcceptH . BC.unpack)

headerParser :: HeaderParser
headerParser = choice [userAgentHParser, acceptParser, hostParser] <* string "\r\n"