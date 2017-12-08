{-# LANGUAGE OverloadedStrings #-}
module JSONParserModule
    (parseMovies
    ,parseActors
     ) where

import DataStructures
import Data.Aeson
import Data.Aeson.Types
import Data.Text
import Control.Monad


parseCinema :: Value -> Parser [Cinema]
parseCinema = withObject "parseCinema" $ \o -> o.: "cinemas"

--parses the Cinema data from the JSON Response
instance FromJSON Cinema where
    parseJSON (Object o) = Cinema <$> o .: "name" <*> o .: "id" <*> o .: "distance"
    parseJSON _ = mzero


--parse the Movie data from the JSON Response
instance FromJSON Movie where
    parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
    parseJSON _ = mzero


--parseMovies :: Value -> Parser [Movie]
--parseMovies = withObject "parseMovies" $ \o -> o.: "results"

parseMovies :: String ->  [Movie]
parseMovies = undefined
--catch connection error

parseActors :: String -> [Actor]
parseActors = undefined
