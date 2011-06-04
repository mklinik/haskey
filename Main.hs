module Main where

-- import qualified Data.Bits as DB
import qualified Network.Socket as NS
-- import qualified Network.BSD as NB
-- import qualified Data.List as DL
import qualified Control.Concurrent as CC
import qualified Control.Exception as CE
import qualified System.IO as IO

import qualified Haskey as H

serverMain :: String       -- ^ port number
           -> IO ()
serverMain port = NS.withSocketsDo $ do
    addrinfos <- NS.getAddrInfo (Just (NS.defaultHints {NS.addrFlags = [NS.AI_PASSIVE]}))
                                Nothing
                                (Just port)
    let serveraddr = head addrinfos
    sock <- NS.socket (NS.addrFamily serveraddr) NS.Stream NS.defaultProtocol
    NS.bindSocket sock (NS.addrAddress serveraddr)
    NS.listen sock 5
    procConnections sock

procConnections :: NS.Socket -> IO ()
procConnections masterSock =
    CE.bracket
        (return masterSock)
        (NS.sClose)
        (\masterSock -> do
            (connSock, clientAddr) <- NS.accept masterSock
            CC.forkIO $ procConnection connSock
            procConnections masterSock)

procConnection :: NS.Socket -> IO ()
procConnection connSock = do
    putStrLn "BEGIN"
    connHandle <- NS.socketToHandle connSock IO.ReadWriteMode
    IO.hSetBuffering connHandle IO.LineBuffering
    messages <- IO.hGetContents connHandle
    findEmptyLine (lines messages)
    IO.hPutStrLn connHandle "HTTP/1.1 303 See Other"
    IO.hPutStrLn connHandle "Location: http://www.google.de/search?q=FLUPILUPI"
    IO.hPutStrLn connHandle ""
    IO.hClose connHandle
    putStrLn "END"
        where findEmptyLine [] = return ()
              findEmptyLine (l:ls) | l == "\r" = return ()
                                   | otherwise = putStrLn l >> findEmptyLine ls
