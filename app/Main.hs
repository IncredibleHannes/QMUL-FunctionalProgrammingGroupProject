module Main where

import HTTPRequestModule
import HTTPRequestModule2
import DataBaseModule
import IOActionModule
import DataStructures
--import JsonParsingModule

main :: IO ()
main = do
  conn <- dbConnect -- DataBaseModule
  _ <- initialiseDB conn -- DataBaseModule
  listOfMovies <- httpGetListOfMovies -- HttpRequestModule
  listofActors <- httpGetListOfActores -- HttpRequestModule
  insertMovieIntoDB conn listOfMovies
  insertActorIntoDB conn listofActors
  actor <- askForActor
  movie <- searchMovieInDB conn actor
  location <- askForLocation
  let listOfCinemas = httpApiCinemaRequest movie location -- HttpRequestModule2
  printCinemas listOfCinemas -- IOModule
