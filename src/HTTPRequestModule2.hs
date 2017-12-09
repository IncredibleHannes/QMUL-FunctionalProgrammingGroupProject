module HTTPRequestModule2
    ( httpGetCinemaList,
      httpApiCinemaRequest
    ) where

import DataStructures
import JSONParserModule
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)
import Control.Monad


cinemaReqURL:: String -> String
cinemaReqURL = (++) "https://api.cinelist.co.uk/search/cinemas/location/"

cinemaMoviesURL:: String -> String
cinemaMoviesURL = (++) "https://api.cinelist.co.uk/get/times/cinema/"


-- | for a given location this function returns a list of cinemas that are nearby
httpGetCinemaList :: String -> IO [Cinema]
httpGetCinemaList loc = fmap parseCinemas (simpleHttp $ cinemaReqURL loc)



httpGetListOfMoives :: Cinema -> IO [Movie]
httpGetListOfMoives (Cinema _ i _) = undefined


httpApiCinemaRequest :: Movie -> String ->  [Cinema]
httpApiCinemaRequest = undefined

--httpApiCinemaRequest :: Movie -> String -> IO [Cinema]
--httpApiCinemaRequest (Movie i n r) l =
--     cinemaList <- httpGetCinemaList l

--     filter (\x -> contains' (httpGetListOfMoives x) n ) cinemaList
--  return

--contains':: Movie -> IO [ManuelMovies] -> IO Bool
