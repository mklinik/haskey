{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network as N
import qualified Network.Socket as NS
import qualified Control.Monad as CM
import qualified Network.HTTP as HTTP
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Control.Exception as CE

defaultAnswer = "<html><head></head><body><h1>haskey</h1><p>by mkl, 2011</p><form action=\"/\" method=\"get\"><input name=\"q\" type=\"text\"/></form></body></html>"

server port = N.withSocketsDo $ do
  CE.bracket
    (N.listenOn $ N.PortNumber port)
    (NS.sClose)
    (\socket -> CM.forever $ do
        (clientSocket, addr) <- NS.accept socket
        print addr
        c <- HTTP.socketConnection "" clientSocket :: IO (HTTP.HandleStream L.ByteString)
        request <- HTTP.receiveHTTP c
        case request of
          Right x -> do
            -- print x
            case mapURL $ HTTP.rqURI x of
              Just url -> do
                putStrLn $ "redirecting to: " ++ (show url)
                HTTP.respondHTTP c $ HTTP.Response (3, 0, 3) "See Other" [(HTTP.Header HTTP.HdrLocation url)] ""
              Nothing  -> do
                HTTP.respondHTTP c $ HTTP.Response (2, 0, 0) "Ok" [] defaultAnswer
            print "DONE"
          Left err -> print err
        HTTP.close c
    )

mapURL url = Just "http://www.google.com/search?q=FLUPILUPI"
