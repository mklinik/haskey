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


defaultAnswer = "<html><head></head><body><h1>haskey</h1><p>by mkl, 2011-2017</p><form action=\"/\" method=\"get\"><input name=\"q\" type=\"text\"/><input type=\"submit\" name=\"mysubmit\" value=\"Submit\" /></form></body></html>"

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
        Right req -> do
          let url = makeURL $ HTTP.rqURI req in
              HTTP.respondHTTP c $ HTTP.Response (3, 0, 3) "See Other" [(HTTP.Header HTTP.HdrLocation url)] ""
        Left err -> print err
      HTTP.close c
    )

makeURL :: NU.URI -> String
makeURL url = prefix ++ (CGI.urlEncode realQuery) ++ suffix
  where
    nameValues = (CGI.formDecode $ dropWhile (=='?') $ NU.uriQuery url)
    query = maybe "" id (lookup "q" nameValues)
    (keyword, realQuery) = maybe ("", query) id (splitAtFirstWS query)
    (prefix,suffix) = maybe ("https://www.startpage.com/do/search?query=","") id (lookup keyword defaultConfig)

-- First entry is keyword
-- Second and third entries are prefix and suffix of search url
type Config = [(String, (String, String))]

defaultConfig :: Config
defaultConfig =
  [ ("g",   ("https://www.startpage.com/do/search?query=",""))
  , ("osm", ("http://www.openstreetmap.org/?query=",""))
  , ("yt",  ("http://www.youtube.com/results?search_query=",""))
  , ("h",   ("http://www.haskell.org/hoogle/?hoogle=",""))
  , ("en",  ("http://www.encyclo.nl/begrip/",""))
  , ("dn",  ("http://m.dict.cc/denl/?s=",""))
  , ("d",   ("http://m.dict.cc/deen/?s=",""))
  , ("ud",  ("http://www.urbandictionary.com/define.php?term=",""))
  , ("w",   ("https://en.wikipedia.org/w/index.php?search=","&title=Special%3ASearch&fulltext=1"))
  , ("wd",  ("https://de.wikipedia.org/wiki/Spezial:Suche/",""))
  , ("wn",  ("https://nl.wikipedia.org/wiki/Special:Search?search=",""))
  , ("gg",  ("http://www.google.com/search?q=",""))
  ]

splitAtFirstWS :: String -> Maybe (String, String)
splitAtFirstWS s = case break DC.isSpace s of
  (_, "") -> Nothing
  (keyword, realQuery) -> Just (keyword, dropWhile DC.isSpace realQuery)
