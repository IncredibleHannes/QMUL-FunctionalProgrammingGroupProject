module Main where

import HTTPRequestModule
import HTTPRequestModule2
import DataBaseModule
import IOActionModule
import DataStructures
import JSONParserModule

main :: IO ()
main = do
  conn <- dbConnect                                         -- DataBaseModule
  _ <- initialiseDB conn                                    -- DataBaseModule
  listOfMovies <- httpGetListOfMovies                       -- HttpRequestModule
  listofActors <- httpGetListOfActores                      -- HttpRequestModule
  insertMovieIntoDB conn listOfMovies                       -- DataBaseModule
  insertActorIntoDB conn listofActors                       -- DataBaseModule
  actor <- askForActor                                      -- IOModule
  movies <- searchMoviesInDB conn actor                     -- DataBaseModule
  selectedMovie <- askToSelectAmovie                        -- IOModule
  let movie = case movies of
                Nothing -> error ""
                Just m  -> m !! selectedMovie
  printMovies movies                                        -- IOModule
  location <- askForLocation                                -- IOModule
  let listOfCinemas = httpApiCinemaRequest movie location   -- HttpRequestModule2
  printCinemas listOfCinemas                                -- IOModule
