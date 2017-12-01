module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores,
      download,
      movieReqURL
    ) where

import Data.List
import DataStructures
import JSONParserModule

import Network.URI
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.Maybe
import Data.Either

movieReqURL :: String
movieReqURL = "https://api.themoviedb.org/3/discover/movie?api_key=77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=popularity.desc&include_adult=false&include_video=false&page=1&primary_release_date.gte=2016-11-26&primary_release_date.lte=2017-11-26"

actorReqURL :: String
actorReqURL = undefined

makeURI :: String -> URI
makeURI str = fromJust $ parseURI $ str

download :: String -> IO String
download str = do
  req <- parseRequest str
  resp <- httpJSON req
  let rBody = responseBody $ resp
  return rBody

httpGetListOfMovies :: IO [Movie]
httpGetListOfMovies = do
  moviesStr <- download movieReqURL
  let movies = parseMovies moviesStr
  return movies

httpGetListOfActores :: IO [Actor]
httpGetListOfActores = do
  actorsStr <- download actorReqURL
  let actors = parseActors actorsStr
  return actors
