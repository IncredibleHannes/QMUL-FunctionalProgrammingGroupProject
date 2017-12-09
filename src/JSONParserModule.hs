{-# LANGUAGE OverloadedStrings #-}
module JSONParserModule
    (parseMovies
    ,parseActors
    ,parseCinemas
    ,parseListings
     ) where

import DataStructures
import Data.Aeson
import Data.Aeson.Types
import Data.Maybe
import Data.Text
import Control.Monad
import qualified Data.ByteString.Lazy as B

listingsParser :: Value -> Parser [Listings]
listingsParser = withObject "listingsParser" $ \o -> o.: "listings"

instance FromJSON Listings where
  parseJSON (Object o) = Listings <$> o .: "title" 
  parseJSON _ = mzero


parseListings :: B.ByteString -> [Listings]
parseListings l = fromJust $ parseMaybe listingsParser =<< decode l

--------------------------------------------------------------------
parseCinemas :: B.ByteString -> [Cinema]
parseCinemas cn = fromJust $ parseMaybe cinemaParser =<< decode cn


cinemaParser :: Value -> Parser [Cinema]
cinemaParser = withObject "cinemaParser" $ \o -> o.: "cinemas"

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
