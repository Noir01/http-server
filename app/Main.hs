{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Concurrent (forkFinally)
import Control.Monad (forever, void)
import qualified Data.ByteString.Char8 as BC
import Network.HTTP.Types (StdMethod (..))
import Network.Socket
  ( AddrInfo (addrAddress, addrFamily),
    Socket,
    SocketType (Stream),
    accept,
    bind,
    close,
    defaultProtocol,
    getAddrInfo,
    gracefulClose,
    listen,
    socket,
    setSocketOption,
    SocketOption (ReuseAddr)
  )
import Network.Socket.ByteString (recv, send)
import Text.Parser (ByteStringWithChars (..))
import Network.Data.Request (Header (UserAgentH, ContentLenghtH, AcceptEncodingH), Req (..), URI (..), emptyReq)
import System.IO (BufferMode (..), hSetBuffering, stdout)
import Text.Megaparsec (parse, many)
import Network.Parser.Request (requestParser)
import Network.Parser.Header (headerParser)
import System.Environment (getArgs)
import System.Directory (doesFileExist)

main :: IO ()
main = do
  args <- getArgs
  let folder = if not (null args)
                then args !! 1
                else "/tmp"
  hSetBuffering stdout LineBuffering

  let host = "127.0.0.1"
      port = "4221"

  BC.putStrLn $ "Listening on " <> BC.pack host <> ":" <> BC.pack port

  -- Get address information for the given host and port
  addrInfo <- getAddrInfo Nothing (Just host) (Just port)

  serverSocket <- socket (addrFamily $ head addrInfo) Stream defaultProtocol
  setSocketOption serverSocket ReuseAddr 1
  bind serverSocket $ addrAddress $ head addrInfo
  listen serverSocket 5

  -- Accept connections and handle them forever
  void . forever $ do
        (clientSocket, clientAddr) <- accept serverSocket
        BC.putStrLn $ "Accepted connection from " <> BC.pack (show clientAddr) <> "."
        -- Handle the clientSocket as needed...
        forkFinally (server clientSocket folder) (const $ gracefulClose clientSocket 5000)
  close serverSocket

server :: Socket -> FilePath -> IO ()
server clientSocket folder = do
        req <- readRequestLine clientSocket
        hs <- readHeaders clientSocket
        if method req == GET
          then resolveGetRequest clientSocket req hs folder
          else if method req == POST
                  then do
                    case findContentSize hs of
                      Just size -> do
                        body <- readBody clientSocket size
                        resolvePostRequest clientSocket req body folder
                      Nothing -> do
                        putStrLn ("POST without content length header: " <> show hs)
                        void $ send clientSocket "HTTP/1.1 400 Bad Request\r\n\r\n"
                  else void $ send clientSocket "HTTP/1.1 405 Method Not Allowed\r\n\r\n"

resolvePostRequest :: Socket -> Req -> BC.ByteString -> FilePath -> IO ()
resolvePostRequest clientSocket req body folder = do
  case uri req of
              File s -> do
                    putStrLn "POST /files"
                    BC.writeFile (folder <> BC.unpack s) body
                    void $ send clientSocket "HTTP/1.1 201 Created\r\n\r\n"
              _ -> do
                putStrLn ("POST something that is not files" <> show req)
                void $ send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"
              
resolveGetRequest :: Socket -> Req -> [Header] -> FilePath -> IO ()
resolveGetRequest clientSocket req hs folder = do
  let compressionScheme = findAcceptEncoding hs
  let contentEncoding = case compressionScheme of
                            Just x -> "Content-Encoding: " <> x
                            Nothing -> ""
  case uri req of
              Home -> void $ send clientSocket ("HTTP/1.1 200 OK\r\n" <> contentEncoding <> "\r\n\r\n")
              Unknown _ -> void $ send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n"
              Echo s -> void $ send clientSocket ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> (BC.pack . show . BC.length) s <> "\r\n" <> contentEncoding <> "\r\n\r\n" <> s)
              UserAgent -> do
                case findUserAgent hs of
                  Nothing -> void $ send clientSocket "HTTP/1.1 400 OK\r\n\r\n"
                  Just ua -> void $ send clientSocket ("HTTP/1.1 200 OK\r\nContent-Type: text/plain\r\nContent-Length: " <> (BC.pack . show . BC.length) ua <> "\r\n" <> contentEncoding <> "\r\n\r\n" <> ua)
              File s -> do
                exists <- doesFileExist (folder <> BC.unpack s)
                if exists
                  then do
                    contents <- BC.readFile (folder <> BC.unpack s)
                    void $ send clientSocket ("HTTP/1.1 200 OK\r\nContent-Type: application/octet-stream\r\nContent-Length:" <> (BC.pack . show . BC.length) contents <> "\r\n" <> contentEncoding <> "\r\n\r\n" <> contents)
                  else void $ send clientSocket "HTTP/1.1 404 Not Found\r\n\r\n" 

findUserAgent :: [Header] -> Maybe BC.ByteString
findUserAgent [] = Nothing
findUserAgent (UserAgentH ua : _) = Just ua
findUserAgent (_ : xs) = findUserAgent xs

findContentSize :: [Header] -> Maybe Int
findContentSize [] = Nothing
findContentSize (ContentLenghtH ua : _) = Just ua
findContentSize (_ : xs) = findContentSize xs

findAcceptEncoding :: [Header] -> Maybe BC.ByteString
findAcceptEncoding [] = Nothing
findAcceptEncoding (AcceptEncodingH ua : _) = Just ua
findAcceptEncoding (_ : xs) = findAcceptEncoding xs

getNextData :: Socket -> IO BC.ByteString
getNextData s = go s ""
  where
    go sInput acc = do
      next <- recv sInput 1
      let end = if acc == "" then next else BC.cons (BC.last acc) next
      if end == "\r\n" then return (acc <> next) else go s (acc <> next)

readRequestLine :: Socket -> IO Req
readRequestLine sock = do
    reqInput <- getNextData sock 
    either (const $ return emptyReq) return . parse requestParser "" . BS $ reqInput

readBody :: Socket -> Int -> IO BC.ByteString
readBody = recv

readHeaders :: Socket -> IO [Header]
readHeaders sock = do
  hs <- readHeadersBytes sock
  either (const $ return []) return . parse (many headerParser) "" . BS $ hs
  where
    readHeadersBytes s = do
      header <- getNextData s
      if header == "\r\n"
        then return ""
        else (header <>) <$> readHeadersBytes s