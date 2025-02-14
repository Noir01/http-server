{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Monad (forever)
import qualified Data.ByteString.Char8 as BC
import Network.Socket
    ( getAddrInfo,
      accept,
      bind,
      listen,
      socket,
      close,
      defaultProtocol,
      AddrInfo(addrAddress, addrFamily),
      SocketType(Stream) )
import Network.Socket.ByteString (send)
import System.IO (BufferMode (..), hSetBuffering, stdout)

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
        _ <- send clientSocket "HTTP/1.1 200 OK\r\n\r\n" 
        close clientSocket
