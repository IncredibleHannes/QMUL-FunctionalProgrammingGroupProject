module DataBaseModule
    ( dbConnect,
      initialiseDB,
      insertMovieIntoDB,
      insertActorIntoDB,
      searchMovieInDB
    ) where

import Database.HDBC
import Database.HDBC.Sqlite3
import Data.List
import DataStructures

dbConnect :: IO Connection
dbConnect = connectSqlite3 "movies.db"

initialiseDB :: Connection -> IO ()
initialiseDB conn = do
   run conn "CREATE TABLE movies (movieId BIGINT(11), name VARCHAR(40))" []
   run conn "CREATE TABLE actores (actorId BIGINT(11), name VARCHAR(40))" []
   run conn "CREATE TABLE plays (actorId BIGINT(11), movieId BIGINT(11))" []
   print "Database initialised!"

insertMovieIntoDB :: Connection -> [Movie] -> IO ()
insertMovieIntoDB conn movie = do
    let f (Movie movieId name) = [toSql movieId, toSql name]
    let args = map f movie
    stmt <- prepare conn "INSERT INTO movies VALUES (?, ?)"
    executeMany stmt args
    print "Movie Database filled with values!"
    commit conn
    print "Commited values to the Database!"

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

searchMovieInDB :: Connection -> String -> IO Movie
searchMovieInDB = undefined
