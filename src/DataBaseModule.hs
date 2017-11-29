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
      searchMoviesInDB
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
   if "prices" `notElem` tables then do
     run conn "CREATE TABLE movies (movieId BIGINT(11), name VARCHAR(40))" []
     run conn "CREATE TABLE actores (actorId BIGINT(11), name VARCHAR(40))" []
     run conn "CREATE TABLE plays (actorId BIGINT(11), movieId BIGINT(11))" []
     commit conn
     print "Database initialised!"
    else
      return ()

-- | Inserts a given list of movies into the movies table
insertMovieIntoDB :: Connection -> [Movie] -> IO ()
insertMovieIntoDB conn movie = do
    let f (Movie movieId name) = [toSql movieId, toSql name]
    let args = map f movie
    stmt <- prepare conn "INSERT INTO movies VALUES (?, ?)"
    executeMany stmt args
    print "Movie Database filled with values!"
    commit conn
    print "Commited values to the Database!"

{- | Inserts a given list of actors into the actores table and fills the plays
     relation table with all actore movie connections -}
insertActorIntoDB :: Connection -> [Actor] -> IO ()
insertActorIntoDB conn actores = do
    -- creating actores table
    let f (Actor actorId name _) = [toSql actorId, toSql name]
    let actoresArgs = map f actores
    stmt <- prepare conn "INSERT INTO actores VALUES (?, ?)"
    executeMany stmt actoresArgs
    -- creating plays table
    let f (Actor actorId n [])     = []
        f (Actor actorId n (x:xs)) = [actorId, x] : f (Actor actorId n xs)
    let playsArgs = concatMap f actores
    executeMany stmt actoresArgs
    print "Actor Database filled with values!"
    commit conn
    print "Commited values to the Database!"

{- | Looksup al movies a given actore plays in and returns a maybe list of movie.
     The maybe will Nothing if there is no movie for a given Actore -}
searchMoviesInDB :: Connection -> String -> IO (Maybe [Movie])
searchMoviesInDB = undefined
