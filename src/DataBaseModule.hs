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
      disconnectDB,
      clearDatabase,
      cleanupDatabase,
      getMoviesFromDatabase,
      getActorsFromDatabase,
      getDateOfLastMoveInDB
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import DataStructures
import Control.Monad

-- | Establishes the connection to our movie database
dbConnect :: IO Connection
dbConnect = connectSqlite3 "movies.db"

{- | Given a connection to a database this function creates all the neccesary
     tables if they are not existing bevore -}
initialiseDB :: Connection -> IO ()
initialiseDB conn = do
   tables <- getTables conn
   run conn ("CREATE TABLE IF NOT EXISTS movies (movieId INT PRIMARY KEY ON " ++
             "CONFLICT IGNORE, name TEXT NOT NULL, release TEXT NOT NULL)") []
   run conn ("CREATE TABLE IF NOT EXISTS actors (actorId INT PRIMARY KEY ON " ++
             "CONFLICT IGNORE, name TEXT NOT NULL)") []
   run conn ("CREATE TABLE IF NOT EXISTS plays (actorId INT NOT NULL, movieId " ++
             "INT NOT NULL, PRIMARY KEY (actorId, movieId) ON CONFLICT IGNORE, " ++
             "FOREIGN KEY (movieId) REFERENCES movies(movieId), FOREIGN KEY " ++
             "(actorId) REFERENCES actors(actorId))") []
   commit conn

-- | Inserts a given list of movies into the movies table
insertMovieIntoDB :: Connection -> [Movie] -> IO ()
insertMovieIntoDB conn movie = do
    let f (Movie movieId name release) = [toSql movieId, toSql name, toSql release]
    let args = map f movie
    stmt <- prepare conn "INSERT INTO movies VALUES (?, ?, ?)"
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
        f (Actor actorId n (Movie movieId _ _ : xs)) = [toSql actorId, toSql movieId]  : f (Actor actorId n xs)
    let playsArgs = concatMap f actores
    stmt <- prepare conn "INSERT INTO plays VALUES (?, ?)"
    executeMany stmt playsArgs
    commit conn

{- | Looksup all movies a given actore plays in and returns a maybe list of movie.
     The maybe will Nothing if there is no movie for a given actore -}
searchMoviesInDB :: Connection -> String -> IO (Maybe [Movie])
searchMoviesInDB conn name = do
  result <- quickQuery' conn ("SELECT movies.* FROM movies, actors, plays " ++
                              "WHERE actors.name == ? " ++
                              "AND actors.actorId = plays.actorId " ++
                              "AND  plays.movieId = movies.movieId") [toSql name]
  case result of
    [] -> return Nothing
    x  -> return (Just $ convertFromSql x)
  where
    convertFromSql :: [[SqlValue]] -> [Movie]
    convertFromSql = map (\x -> Movie (fromSql $ head x) (fromSql $ x !! 1)
                         (fromSql $ x !! 2))

-- | Closes the database connection
disconnectDB :: Connection -> IO ()
disconnectDB = disconnect

-- | drops all tables in the database
clearDatabase :: Connection -> IO()
clearDatabase conn = do
   run conn "DROP TABLE IF EXISTS movies" []
   run conn "DROP TABLE IF EXISTS actors" []
   run conn "DROP TABLE IF EXISTS plays" []
   commit conn

-- | looks up all movies in the database and returns a list of all actores
getMoviesFromDatabase :: Connection -> IO [Movie]
getMoviesFromDatabase conn = do
  result <- quickQuery' conn "SELECT * FROM movies" []
  return $ map (\x -> Movie (fromSql $ head x) (fromSql $ x !! 1)
                (fromSql $ x !! 2)) result

-- | looks up all actores in the database and returns a list of all actores
getActorsFromDatabase :: Connection -> IO [Actor]
-- make the actor array correctly
getActorsFromDatabase conn = do
  result <- quickQuery' conn "SELECT * FROM actors" []
  let ids = map head result
  result2 <- mapM (\x -> quickQuery' conn ("SELECT movies.* FROM movies, plays " ++
              "WHERE ? = plays.actorId AND plays.movieId = movies.movieId") [x]) ids
  let finalResult = zip result result2
  return $ map (\x -> Actor (fromSql $ head $ fst x) (fromSql $ fst x !! 1) (map parseMovie (snd x))) finalResult

parseMovie :: [SqlValue] -> Movie
parseMovie x = Movie (fromSql $ head x) (fromSql $ x !! 1) (fromSql $ x !! 2)

{- | removes all movies bevor a given date and deals with inonsistent states in the
     database after the deletion of a movie-}
cleanupDatabase :: Connection -> String -> IO ()
cleanupDatabase conn date = do
   result <- quickQuery' conn ("SELECT movies.movieId FROM movies WHERE " ++
              "movies.release < ?") [toSql date]
   stmt <- prepare conn "DELETE FROM movies WHERE movies.movieId = ?"
   executeMany stmt result
   stmt <- prepare conn "DELETE FROM plays WHERE plays.movieId = ?"
   executeMany stmt result
   run conn ("DELETE FROM actors WHERE NOT EXISTS (SELECT * FROM plays WHERE " ++
             "actors.actorId == plays.actorId)") []
   commit conn

getDateOfLastMoveInDB :: Connection -> IO (Maybe String)
getDateOfLastMoveInDB conn = do
  result <- quickQuery' conn "SELECT MAX(movies.release) FROM movies" []
  if result == [[]] then return Nothing else return $ fromSql $ head $ head result
