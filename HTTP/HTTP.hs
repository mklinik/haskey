module Haskey.HTTP where

import qualified Data.String.Utils as SU

-- splits the GET line of a HTTP request into cgi parameters and values
-- performs NO url decoding
-- "GET /?a=b&c=d HTTP/1.1" |-> [("a", "b"), ("c", "d")]
parseGetRequest :: String -> Maybe [(String,String)]
parseGetRequest req = let parts = words req in
    if length parts == 3
        then parseUrl (parts !! 1)
        else Nothing

parseUrl :: String -> Maybe [(String, String)]
parseUrl url = let xs = concat $ map (SU.split "=") $ SU.split "&" ((SU.split "?" url) !! 1) in
    if length xs `mod` 2 == 1
        then Nothing
        else Just $ zip (evenIndexes xs) (oddIndexes xs)

oddIndexes :: [a] -> [a]
oddIndexes xs = oddIndexes' xs 0

oddIndexes' :: [a] -> Integer -> [a]
oddIndexes' [] _ = []
oddIndexes' (x:xs) n = if n `mod` 2 == 1
    then (x : (oddIndexes' xs (n + 1)))
    else (oddIndexes' xs (n + 1))

evenIndexes :: [a] -> [a]
evenIndexes xs = evenIndexes' xs 0

evenIndexes' :: [a] -> Integer -> [a]
evenIndexes' [] _ = []
evenIndexes' (x:xs) n = if n `mod` 2 == 0
    then (x : (evenIndexes' xs (n + 1)))
    else (evenIndexes' xs (n + 1))
