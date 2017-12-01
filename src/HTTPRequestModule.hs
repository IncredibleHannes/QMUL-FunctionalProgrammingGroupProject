module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores
    ) where

import Data.List
import DataStructures
import JSONParserModule

httpGetListOfMovies = undefined
httpGetListOfActores = undefined


{-
import Network.URI
import Network.HTTP
import Data.Maybe
import Data.Either
movieReqURL :: String
MovieReqURL = "https://api.themoviedb.org/3/discover/movie?api_key=77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=popularity.desc&include_adult=false&include_video=false&page=1&primary_release_date.gte=2016-11-26&primary_release_date.lte=2017-11-26"

actorReqURL :: String
actorReqURL = undefined

makeURI :: String -> URI
makeURI str = fromJust $ parseURI $ str

download :: String -> IO Response
download str = do
	resp <- simpleHTTP request
	return resp
    where
    	request = Request {
    	    rqURI = makeURI $ str,
    	    rqMethod = GET,
    	    rqHeaders = [],
    	    rqBody = ""
           }

httpGetListOfMovies :: IO [Movie]
httpGetListOfMovies = do
	moviesStr <- download movieReqURL
	movies :: [Movie] = map parseMovies movieStr
    return movies

httpGetListOfActores :: IO [Actor]
httpGetListOfActores = do
	actorsStr <- download actorReqURL
	actors :: [Actor] = map parseActors actorsStr
    return actors
-}
