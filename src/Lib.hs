module Lib
    ( runServer
    ) where

import Network.Socket

runServer :: PortNumber -> IO ()
runServer port =
    do
        address <- resolve port
        socket_ <- open address port
        mainLoop socket_

resolve :: PortNumber -> IO AddrInfo
resolve port =
    let
        port_string = show port
        address_info_hint = defaultHints 
            { addrFlags = [ AI_Passive ]
            , addrFamily = AF_INET
            , addrSocketType = Stream
            }
    in do
        address:_ <- getAddrInfo (Just address_info_hint) Nothing (Just port_string)
        return address

open :: AddrInfo -> PortNumber -> IO Socket
open address port =
    let
        max_queued_connections = 2
    in do
        socket_ <- socket (addrFamily address) (addrSocketType address) (addrProtocol address)
        setSocketOption socket_ ReuseAddr 1
        bind socket_ $ SockAddrInet port (addrAddress address)
        listen socket_ max_queued_connections
        return socket_

mainLoop :: Socket -> IO ()
mainLoop chat_socket = do
    connection <- accept chat_socket
    handleConnection connection
    mainLoop chat_socket
    

handleConnection :: (Socket, SockAddr) -> IO ()
handleConnection (chat_socket, _) = do
    send chat_socket "Servus!\n"
    close chat_socket


