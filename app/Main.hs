{- |
   Module     : Main
   Copyright  : Copyright (C) 2017 Johannes Hartmann, Liam Kelly, Manuel Campos Villarreal
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This application is downloading all recent movies from the TheMovieDB api and
stores it into a SQLite database. The user types in an actor and this application
is looking this actor up and print out all movies he's playing in. For a given
location it suggests cinemas in the areas that play this movie.

Written by Johannes Hartmann, Liam Kelly, Manuel Campos Villarreal
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
import Control.Monad
import qualified Network.HTTP.Conduit as N

run :: IO ()
run = do
  conn <- dbConnect
  initialiseDB conn
  --cleanupDatabase conn "2017-11-01"
  lastMovieDate <- getDateOfLastMoveInDB conn

  let movieHandle = (\e -> return []) :: N.HttpException -> IO [Movie]
  listOfMovies <- handle movieHandle
                  (httpGetListOfMovies $ fromMaybe "2017-11-12" lastMovieDate)
  let actorHandle = (\e -> return []) :: N.HttpException -> IO [Actor]
  listOfActors <- handle actorHandle (httpGetListOfActores listOfMovies)
  insertMovieIntoDB conn listOfMovies
  insertActorIntoDB conn listOfActors
  movies <- getMoviesFromDatabase conn
  actoreName <- askForActor
  movie <- searchMoviesInDB conn actoreName
  case movie of
    Nothing -> do
      disconnectDB conn
      error "Could't find a movie for the given actor"
    Just x -> printMovies x
  movieIndex <- askToSelectMovie
  when (movieIndex > length (fromJust movie)) $ disconnectDB conn
    >>= error "Wrong Input"
  location <- askForLocation
  let cinemaHandle = (\e -> disconnectDB conn >>= error "No cinema found for your location") :: N.HttpException -> IO [Cinema]
  cinemas <- handle cinemaHandle (httpGetCinemaList location)
  let cinema2Handle = (\e -> disconnectDB conn >>= error "No cinema found that plays your movie") :: N.HttpException -> IO [Cinema]
  filteredCinemas <- handle cinema2Handle (httpApiCinemaRequest
    (fromJust movie !! (movieIndex - 1)) cinemas)
  printCinemas filteredCinemas
  disconnectDB conn

main :: IO ()
main = do
  let mainHandler = (print . takeWhile (/= '\n') . show) :: SomeException -> IO ()
  handle mainHandler run
