module HTTPRequestModule2
    ( requestCinemaList,
      httpApiCinemaRequest
    ) where

import DataStructures
import JSONParserModule
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)


cinemaReqURL:: String->String
cinemaReqURL loc= "https://api.cinelist.co.uk/search/cinemas/location/" ++ loc

getJSON ::String->IO B.ByteString
getJSON reqUrl= simpleHttp reqUrl

requestCinemaList :: String -> IO B.ByteString
requestCinemaList loc = do
  response <- (getJSON (cinemaReqURL loc))
  return response

httpApiCinemaRequest :: Movie -> String -> [Cinema]

httpApiCinemaRequest = undefined
