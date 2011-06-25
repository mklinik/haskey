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


defaultAnswer = "<html><head></head><body><h1>haskey</h1><p>by mkl, 2011</p><form action=\"/\" method=\"get\"><input name=\"q\" type=\"text\"/><input name=\"c\" type=\"text\"/><input type=\"submit\" name=\"mysubmit\" value=\"Submit\" /></form></body></html>"

main = server 8080

server port = N.withSocketsDo $ do
  CE.bracket
    (N.listenOn $ N.PortNumber port)
    (NS.sClose)
    (\socket -> CM.forever $ do
        (clientSocket, addr) <- NS.accept socket
        c <- HTTP.socketConnection "" clientSocket :: IO (HTTP.HandleStream L.ByteString)
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
        item <- lookup keyword $ lookupConfig nameValues
        return $ item ++ (CGI.urlEncode realQuery)

type Config = [(String, String)]

defaultConfig :: Config
defaultConfig =
    [ ("g", "http://www.google.com/search?q=")
    , ("gm", "http://maps.google.de/maps?q=")
    , ("y", "http://de.search.yahoo.com/search?p=")
    , ("yt", "http://www.youtube.com/results?search_query=")
    , ("d", "http://dict.leo.org/?search=")
    , ("h", "http://www.haskell.org/hoogle/?hoogle=")
    , ("ex", "https://addons.mozilla.org/en-US/firefox/search?q=")
    , ("wd", "http://de.wikipedia.org/wiki/Spezial:Search?search=")
    , ("w", "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=")
    , ("osm", "http://www.openstreetmap.org/?query=")
    , ("h", "http://www.haskell.org/hoogle/?hoogle=")
    ]

mklConfig :: Config
mklConfig =
    [ ("gh", "https://github.com/search?type=Everything&q=")
    , ("ft", "http://www.filestube.com/search.html?select=All&q=")
    , ("gd", "http://www.google.de/search?q=")
    ] ++ defaultConfig

solConfig :: Config
solConfig =
    [ ("g", "http://en.wikipedia.org/w/index.php?title=Special%3ASearch&search=")
    ]

-- configs is an association list
configs :: [(String, Config)]
configs = [("mkl", mklConfig), ("sol", solConfig)]

-- when there is no config with that name in configs, use defaultConfig
lookupConfig :: [(String, String)] -> Config
lookupConfig nameValues =
    case lookup "c" nameValues >>= (flip lookup) configs of
        Just c -> c
        Nothing -> defaultConfig

splitAtFirstWS :: String -> Maybe (String, String)
splitAtFirstWS s = case break DC.isSpace s of
    (_, "") -> Nothing
    (keyword, realQuery) -> Just (keyword, dropWhile DC.isSpace realQuery)
