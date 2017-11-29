{- |
   Module     : DataBaseModule
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This module handles all database related parts of this application.

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}

module DataBaseModule
    ( dbConnect,
      initialiseDB,
      insertMovieIntoDB,
      insertActorIntoDB,
      searchMoviesInDB,
      disconnectDB
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import DataStructures

-- | Establishes the connection to our movie database
dbConnect :: IO Connection
dbConnect = connectSqlite3 "movies.db"

{- | Given a connection to a database this function creates all the neccesary
     tables if they are not existing bevore -}
initialiseDB :: Connection -> IO ()
initialiseDB conn = do
   tables <- getTables conn
   run conn "DROP TABLE IF EXISTS movies" []
   run conn "DROP TABLE IF EXISTS actors" []
   run conn "DROP TABLE IF EXISTS plays" []
   run conn "CREATE TABLE movies (movieId INT PRIMARY KEY, name TEXT NOT NULL)" []
   run conn "CREATE TABLE actors (actorId INT PRIMARY KEY, name TEXT NOT NULL)" []
   run conn "CREATE TABLE plays (actorId INT NOT NULL, movieId INT NOT NULL, PRIMARY KEY (actorId, movieId) FOREIGN KEY (movieId) REFERENCES movies(movieId), FOREIGN KEY (actorId) REFERENCES actors(actorId))" []
   commit conn

-- | Inserts a given list of movies into the movies table
insertMovieIntoDB :: Connection -> [Movie] -> IO ()
insertMovieIntoDB conn movie = do
    let f (Movie movieId name) = [toSql movieId, toSql name]
    let args = map f movie
    stmt <- prepare conn "INSERT INTO movies VALUES (?, ?)"
    executeMany stmt args
    commit conn

{- | Inserts a given list of actors into the actores table and fills the plays
     relation table with all actore movie connections -}
insertActorIntoDB :: Connection -> [Actor] -> IO ()
insertActorIntoDB conn actores = do
    -- creating actores table
    let f (Actor actorId name _) = [toSql actorId, toSql name]
    let actoresArgs = map f actores
    stmt <- prepare conn "INSERT INTO actors VALUES (?, ?)"
    executeMany stmt actoresArgs
    -- creating plays table
    let f (Actor actorId n [])     = []
        f (Actor actorId n (x:xs)) = [toSql actorId, toSql x] : f (Actor actorId n xs)
    let playsArgs = concatMap f actores
    stmt <- prepare conn "INSERT INTO plays VALUES (?, ?)"
    executeMany stmt playsArgs
    commit conn

{- | Looksup al movies a given actore plays in and returns a maybe list of movie.
     The maybe will Nothing if there is no movie for a given Actore -}
searchMoviesInDB :: Connection -> String -> IO (Maybe [Movie])
searchMoviesInDB conn name = do
  result <- quickQuery' conn ("SELECT movies.* FROM movies, actors, plays WHERE actors.name == ? " ++
                              "AND actors.actorId = plays.actorId " ++
                              "AND  plays.movieId = movies.movieId") [toSql name]
  case result of
    [] -> return Nothing
    x  -> return (Just $ convertFromSql x)
  where
    convertFromSql :: [[SqlValue]] -> [Movie]
    convertFromSql = map (\x -> Movie (fromSql $ head x) (fromSql $ x !! 1))

disconnectDB :: Connection -> IO ()
disconnectDB = disconnect
