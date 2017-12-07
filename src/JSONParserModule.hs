{-# LANGUAGE OverloadedStrings #-}
module JSONParserModule
    (parseMovies
    ,parseActors
     ) where

import DataStructures
import Data.Aeson
import Data.Text
import Control.Monad

--parses the CinemaList array from the JSON Response
instance FromJSON CinemaList where
    parseJSON (Object o) = CinemaList <$> o .: "cinemas"
    parseJSON _ = mzero

--parses the Cinema data from the JSON Response
instance FromJSON Cinema where
    parseJSON (Object o) = Cinema <$> o .: "name" <*> o .: "id" <*> o .: "distance"
    parseJSON _ = mzero

--parses the Movie results array from the JSON Response
instance FromJSON MovieList where
    parseJSON (Object o) = MovieList <$> o .: "results"
    parseJSON _ = mzero

--parse the Movie data from the JSON Response
instance FromJSON Movie where
    parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
    parseJSON _ = mzero



parseMovies :: String -> [Movie]
parseMovies = undefined
--catch connection error

parseActors :: String -> [Actor]
parseActors = undefined
