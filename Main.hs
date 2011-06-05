module Main where

-- import qualified Data.Bits as DB
import qualified Network.Socket as NS
-- import qualified Network.BSD as NB
-- import qualified Data.List as DL
import qualified Control.Concurrent as CC
import qualified Control.Exception as CE
import qualified System.IO as IO

import qualified Haskey as H
import qualified HTTP.HTTP as HHTTP

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
    let answer = answerQuery $ lines messages
    IO.hPutStr connHandle answer
    IO.hClose connHandle
    putStrLn "END"

findEmptyLine [] = return ()
findEmptyLine (l:ls) | l == "\r" = return ()
                     | otherwise = putStrLn l >> findEmptyLine ls

answerQuery :: [String] -> String
answerQuery (x:xs) = case HHTTP.parseGetRequest x of
    Just _ -> "HTTP/1.1 303 See Other\r\nLocation: http://www.google.de/search?q=FLUPILUPI\r\n\r\n"
    Nothing -> "HTTP/1.1 200 Ok\r\nConnection: close\r\n\r\n<html><head></head><body><h1>haskey</h1><p>by mkl, 2011</p><form action=\"/\" method=\"get\"><input name=\"q\" type=\"text\"/></form></body></html>"
