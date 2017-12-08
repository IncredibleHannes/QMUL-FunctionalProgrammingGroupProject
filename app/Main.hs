{- |
   Module     : Main
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This application is downloading all recent movies from the TheMovieDB api and stores it into a
SQLite database. The user types in an actor ant this applicatio is looking him up and printMovies
out all movies he's playing in. For a given location it suggests cinemas in the areas that play
this movie.

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}

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
  listOfMovies <- httpGetListOfMovies "2017-12-01"          -- HttpRequestModule
  --listofActors <- httpGetListOfActores                      -- HttpRequestModule
  insertMovieIntoDB conn listOfMovies                       -- DataBaseModule
  movies <- getMoviesFromDatabase conn
  print movies
  {-insertActorIntoDB conn listofActors                       -- DataBaseModule
  actor <- askForActor                                      -- IOModule
  movies <- searchMoviesInDB conn actor                     -- DataBaseModule
  selectedMovie <- askToSelectAmovie                        -- IOModule
  let movie = case movies of
                Nothing -> error ""
                Just m  -> m !! selectedMovie
  printMovies movies                                        -- IOModule
  location <- askForLocation                                -- IOModule
  let listOfCinemas = httpApiCinemaRequest movie location   -- HttpRequestModule2
  printCinemas listOfCinemas                                -- IOModule-}
  disconnectDB conn                                         -- DataBaseModule
