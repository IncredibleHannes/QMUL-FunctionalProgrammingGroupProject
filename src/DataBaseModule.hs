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
dbConnect = undefined

initialiseDB :: Connection -> IO ()
initialiseDB = undefined

insertMovieIntoDB :: Connection -> [Movie] -> IO ()
insertMovieIntoDB = undefined

insertActorIntoDB :: Connection -> [Actor] -> IO ()
insertActorIntoDB = undefined

searchMovieInDB :: Connection -> String -> IO Movie
searchMovieInDB = undefined
