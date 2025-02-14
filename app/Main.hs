{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getAddrInfo,
    listen,
    socket,
  )
import Network.Socket.ByteString (recv, send)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Megaparsec (parse)
import Network.HTTP.Types (StdMethod(..))
import Parser (requestParser, ByteStringWithChars(..))
import Request (Req(..), emptyReq, URI(..))
import qualified Numeric as BC

main :: IO ()
main = do
  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  forever $ do
    (clientSocket, clientAddr) <- accept serverSocket
    BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
    -- Handle the clientSocket as needed...
    reqInput <- getNextData clientSocket
    BC.putStrLn ("input: " <> reqInput)
    req <- either (const $ return emptyReq) return . parse requestParser "" . BS $ reqInput
    BC.putStrLn $ "URI: " <> BC.pack (show req)
    if method req == GET
        then do
            case uri req of
              HOME -> do
                _ <- send clientSocket "HTTP/1.1 200 OK\r\n\r\n"
                pure ()
              UNKNOWN _ -> do
                _ <- send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"
                pure ()
              ECHO s -> do
                _ <- send clientSocket $ "HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: "<> (BC.pack . show . BC.length) s <> "\r\n\r\n" <> s
                pure ()
            close clientSocket
        else do
            _ <- send clientSocket "HTTP/1.1 405 Method Not Allowed\r\n\r\n"
            close clientSocket

getNextData :: Socket -> IO BC.ByteString
getNextData s = go s ""
  where
    go sInput acc = do
      next <- recv sInput 1
      let end = if acc == "" then next else BC.cons (BC.last acc) next
      if end == "\r\n" then return (BC.concat [acc, next]) else go s (BC.concat [acc, next])