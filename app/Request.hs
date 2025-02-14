{-# LANGUAGE OverloadedStrings #-}

module Request (Req (..), emptyReq, URI (..), Header (..)) where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Types (HttpVersion (..), StdMethod (..), http09)

data URI = Home | Echo BC.ByteString | UserAgent | Unknown BC.ByteString
  deriving (Show)

data Header = HostH BC.ByteString | ContentTypeH String | ContentLenghtH Int | AcceptH String | UserAgentH BC.ByteString | CustomHeader String BC.ByteString
  deriving (Show)

data Req = Req
  {
    method :: StdMethod,
    uri :: URI,
    httpVersion :: HttpVersion
  }
  deriving (Show)

emptyReq :: Req
emptyReq = Req {method = GET, uri = Unknown "", httpVersion = http09}