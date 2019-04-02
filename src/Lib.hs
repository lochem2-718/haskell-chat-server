module Lib
    ( runServer
    ) where

import Network.Socket

runServer :: Int -> IO ()
runServer port_number = do
    chat_socket <- socket AF_INET Stream 0
    setSocketOption chat_socket ReuseAddr 1
    bind chat_socket (SockAddrInet port_number iNADDR_ANY)
    listen chat_socket 2
    mainLoop chat_socket

mainLoop :: Socket -> IO ()
mainLoop chat_socket = do
    connection <- accept chat_socket
    handleConnection connection
    mainLoop chat_socket
    

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (chat_socket, _) = do
    send chat_socket "Servus!\n"
    close chat_socket


