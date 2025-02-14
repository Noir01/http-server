{-# LANGUAGE OverloadedStrings #-}

module Request (Req(..), emptyReq, URI(..)) where

import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Types (StdMethod(..), HttpVersion(..), http09)

data URI = HOME | ECHO BC.ByteString | UNKNOWN BC.ByteString
    deriving Show

data Req = Req {
    method :: StdMethod,
    uri :: URI,
    httpVersion :: HttpVersion
} deriving Show


emptyReq :: Req
emptyReq = Req {method=GET, uri=UNKNOWN "", httpVersion = http09 }