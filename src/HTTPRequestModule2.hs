module HTTPRequestModule2
    ( httpGetCinemaList,
      httpApiCinemaRequest,
      httpGetCinemaListings,
      getJSON,
      callMoviesAPi
    ) where

import DataStructures
import JSONParserModule
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp)


cinemaReqURL:: String->String
cinemaReqURL loc= concat ["https://api.cinelist.co.uk/search/cinemas/location/", loc]

cinemaMoviesURL:: String -> String
cinemaMoviesURL cinemaID = concat ["https://api.cinelist.co.uk/get/times/cinema/",cinemaID]

getJSON ::String->IO B.ByteString
getJSON reqUrl= simpleHttp reqUrl


httpGetCinemaList :: String->IO [Cinema]
httpGetCinemaList loc = do
  cinemaResponse <- (getJSON(cinemaReqURL loc))
  let cinemas = parseCinemas cinemaResponse
  return cinemas


--Test to get CinemaListings
httpGetCinemaListings :: String -> IO [Listings]
httpGetCinemaListings cineID = do
  listingsResponse <- (getJSON(cinemaMoviesURL cineID))
  let cinemaListing = parseListings listingsResponse
  return cinemaListing


-- Test only
callMoviesAPi = do
  cinemaList <- httpGetCinemaList "Stratford"
  cinemaMovies  <- mapM (\(Cinema _ x _) -> httpGetCinemaListings x) cinemaList
  print cinemaMovies



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
