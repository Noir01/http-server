{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Parser (requestParser, Req(..), emptyReq, ByteStringWithChars(..)) where

import Text.Megaparsec (choice, many, Parsec, satisfy, takeWhileP)
import Data.Char (isDigit)
import Text.Megaparsec.Char (string, char)
import Text.Megaparsec.Stream (Stream(..), ShareInput(..), Token, Tokens, VisualStream(..))
import Text.Megaparsec.Debug (dbg')
import Data.Proxy
import qualified Data.List.NonEmpty as NE (toList)
import Data.Void(Void)
import Data.Functor (($>))
import Data.Bifunctor (second)
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Types (StdMethod(..), HttpVersion(..), http09)

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

type URIParser = Parsec Void ByteStringWithChars BC.ByteString

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
methodParser = dbg' "method" $ choice [getParser, postParser, patchParser, putParser, optionsParser, deleteParser, connectParser, traceParser, headParser]

httpVersionParser :: HTTPVersionParser
httpVersionParser = string "HTTP/" *> many (satisfy isDigit) >>= (\major -> char '.' *> many (satisfy isDigit) >>= (\minor -> pure $ HttpVersion (maybe (error "Unexpected") fst . BC.readInt $ BC.pack major) (maybe (error "Unexpected") fst . BC.readInt $ BC.pack minor)))

uriParser :: URIParser
uriParser = takeWhileP Nothing (\tok -> tok `elem` ['\r', '\n', ' '])

data Req = Req {
    method :: StdMethod,
    uri :: BC.ByteString,
    httpVersion :: HttpVersion
} deriving Show

type ReqParser = Parsec Void ByteStringWithChars Req 

requestParser :: ReqParser
requestParser = do
    m <- methodParser
    _ <- char ' '
    u <- uriParser
    _ <- char ' '
    v <- httpVersionParser
    _ <- string "\r\n"
    return $ Req{method = m, uri = u, httpVersion = v}

emptyReq :: Req
emptyReq = Req {method=GET, uri="", httpVersion = http09 }