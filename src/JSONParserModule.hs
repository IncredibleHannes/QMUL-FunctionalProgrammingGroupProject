
{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSONParserModule
    (parseMovies
    ,parseActors
    , parsePages
    , MovieFromJSON
    , Results
     ) where

import DataStructures

import Data.Aeson
import Data.Aeson.Types
import qualified Data.Text as T
import Data.Functor
import Control.Applicative
import Control.Monad
import qualified Data.ByteString.Lazy as B
import Data.Maybe
import GHC.Generics

data Results =
  Results {id :: Int
        , title :: T.Text
        , release_date :: T.Text
         } deriving (Show, Generic)

instance FromJSON Results where
   parseJSON (Object v) = Results <$>
                          v .: "id" <*>
                          v .: "title" <*>
                          v .: "release_date"
   parseJSON _ = mzero

instance ToJSON Results where
   toJSON (Results id title release_date) = object ["id" .= id, "title" .= title, "release_date" .= release_date]

data MovieFromJSON =
    MovieFromJSON {results :: [Results]
                   , pages :: Int} deriving (Show, Generic)

instance FromJSON MovieFromJSON where
   parseJSON (Object v) = MovieFromJSON <$>
                           v .: "results" <*>
                           v .: "total_pages"
   parseJSON _ = mzero

instance ToJSON MovieFromJSON where
	toJSON (MovieFromJSON results pages) = object ["results" .= results, "total_pages" .= pages]

parsePages :: B.ByteString -> Int
parsePages b = pages fromJSON
   where fromJSON = fromJust $ decode b

convertMovie :: Results -> Movie
convertMovie (Results i t r) = Movie i (T.unpack t) (T.unpack r)

parseMovies :: B.ByteString -> [Movie]
parseMovies b = map convertMovie moviesText
  where fromJSON = fromJust $ decode b
        moviesText = results fromJSON

--catch connection error

parseActors :: String -> [Actor]
parseActors = undefined
