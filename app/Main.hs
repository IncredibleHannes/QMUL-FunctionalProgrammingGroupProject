{- |
   Module     : Main
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This application is downloading all recent movies from the TheMovieDB api and
stores it into a SQLite database. The user types in an actor and this application
is looking this actor up and print out all movies he's playing in. For a given
location it suggests cinemas in the areas that play this movie.

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}

module Main where

import HTTPRequestModule
import HTTPRequestModule2
import DataBaseModule
import IOActionModule
import DataStructures
import JSONParserModule

import Data.Maybe
import Control.Exception
import qualified Network.HTTP.Conduit as N

main :: IO ()
main = do
  conn <- dbConnect
  initialiseDB conn
  cleanupDatabase conn "2017-12-06"
  lastMovieDate <- getDateOfLastMoveInDB conn

  let movieHandle = (\e -> return []) :: N.HttpException -> IO [Movie]
  listOfMovies <- handle movieHandle
                  (httpGetListOfMovies $ fromMaybe "2017-12-07" lastMovieDate)
  let actorHandle = (\e -> return []) :: N.HttpException -> IO [Actor]
  listOfActors <- handle actorHandle (httpGetListOfActores listOfMovies)
  insertMovieIntoDB conn listOfMovies
  insertActorIntoDB conn listOfActors
  actoreName <- askForActor
  movie <- searchMoviesInDB conn actoreName
  printMovies movie
  let cinemaHandle = (\e -> return []) :: N.HttpException -> IO [Cinema]
  location <- askForLocation
  result <- handle cinemaHandle (httpGetCinemaList location)
  printCinemas result
  
  disconnectDB conn
  {-
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
  disconnectDB conn   -}                                      -- DataBaseModule
