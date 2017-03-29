{-# LANGUAGE OverloadedStrings #-}
module Main where

import qualified Network as N
import qualified Network.URI as NU
import qualified Network.Socket as NS
import qualified Network.CGI as CGI
import qualified Control.Monad as CM
import qualified Network.HTTP as HTTP
import qualified Data.ByteString.Lazy.Char8 as L
import qualified Control.Exception as CE
import qualified Data.Char as DC
import Text.Regex (mkRegex, subRegex)


defaultAnswer = "<html><head></head><body><h1>haskey</h1><p>by mkl, 2011</p><form action=\"/\" method=\"get\"><input name=\"q\" type=\"text\"/><input name=\"c\" type=\"text\"/><input type=\"submit\" name=\"mysubmit\" value=\"Submit\" /></form></body></html>"

main = server 8080

server port = N.withSocketsDo $ do
  CE.bracket
    (N.listenOn $ N.PortNumber port)
    (NS.sClose)
    (\socket -> CM.forever $ do
        (clientSocket, addr) <- NS.accept socket
        print addr
        c <- HTTP.socketConnection "" 0 clientSocket :: IO (HTTP.HandleStream L.ByteString)
        request <- HTTP.receiveHTTP c
        case request of
          Right x -> do
            case mapURL $ HTTP.rqURI x of
              Just url -> do
                HTTP.respondHTTP c $ HTTP.Response (3, 0, 3) "See Other" [(HTTP.Header HTTP.HdrLocation url)] ""
              Nothing  -> do
                HTTP.respondHTTP c $ HTTP.Response (2, 0, 0) "Ok" [] defaultAnswer
          Left err -> print err
        HTTP.close c
    )

mapURL :: NU.URI -> Maybe String
mapURL url =
    let nameValues = (CGI.formDecode $ dropWhile (=='?') $ NU.uriQuery url)
    in do
        query <- lookup "q" nameValues
        (keyword, realQuery) <- splitAtFirstWS query
        item <- lookup keyword defaultConfig
        return $ subRegex (mkRegex "%s") item (CGI.urlEncode realQuery)

type Config = [(String, String)]

defaultConfig :: Config
defaultConfig =
    [ ("g", "http://www.google.com/search?q=%s")
    , ("gm", "http://maps.google.de/maps?q=%s")
    , ("y", "http://de.search.yahoo.com/search?p=%s")
    , ("yt", "http://www.youtube.com/results?search_query=%s")
    , ("d", "http://dict.leo.org/?search=%s")
    , ("h", "http://www.haskell.org/hoogle/?hoogle=%s")
    , ("ex", "https://addons.mozilla.org/en-US/firefox/search?q=%s")
    , ("wd", "http://de.wikipedia.org/wiki/Spezial:Search?search=%s")
    , ("w", "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=%s")
    , ("osm", "http://www.openstreetmap.org/?query=%s")
    , ("h", "http://www.haskell.org/hoogle/?hoogle=%s")
    , ("sp", "https://www.startpage.com/do/search?query=%s")
    , ("en", "http://www.encyclo.nl/begrip/%s")
    , ("denl", "http://m.dict.cc/denl/?s=%s")
    ]

-- urbandictionary
-- openstreetmap
-- dict.cc denl deen
-- wikipedia de nl en

splitAtFirstWS :: String -> Maybe (String, String)
splitAtFirstWS s = case break DC.isSpace s of
    (_, "") -> Nothing
    (keyword, realQuery) -> Just (keyword, dropWhile DC.isSpace realQuery)
