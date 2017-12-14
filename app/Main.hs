{- |
   Module     : Main
   Copyright  : Copyright (C) 2017 Johannes Hartmann, Liam Kelly, Manuel Campos Villarreal
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

   This purpose of the application is to download all the recent movies from the TheMovieDB API and
   stores them into a SQLite database.
   Main Functionality is as Follows: The user types in an actor in the Console; The application
   will look this actor up and print out all movies he's playing in.
   Then, for a given location it suggests cinemas in the areas that play this movie.

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
import Data.DateTime

{- | The real application function. This function is responsible for filling up the database and interacting with the user.
     This function throws lots of exceptions witch will be handled in
     the main function -}
run :: IO ()
run = do
  conn <- dbConnect
  initialiseDB conn
  date <- getCurrentTime

  let lastMonthDay = getDateString (addMinutes (-2 * 30 * 24 * 60) date) -- two months
  cleanupDatabase conn (getDateString (addMinutes (-6 * 30 * 24 * 60) date)) -- six months
  lastMovieDate <- getDateOfLastMoveInDB conn
  let movieHandle = (\e -> return []) :: N.HttpException -> IO [Movie]
  listOfMovies <- handle movieHandle
                  (httpGetListOfMovies $ fromMaybe lastMonthDay lastMovieDate)
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

-- | The main function of this application
main :: IO ()
main = do
  let mainHandler = (print . takeWhile (/= '\n') . show) :: SomeException -> IO ()
  handle mainHandler run
