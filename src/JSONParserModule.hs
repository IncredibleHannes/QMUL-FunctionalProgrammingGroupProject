{- |
   Module     : JSONParserModule
   Copyright  : Copyright (C) 2017  Liam Kelly, Manuel Campos Villarreal, Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This module takes care of the json parsing for each http module

Written by Liam Kelly, Manuel Campos Villarreal, Johannes Hartmann
-}

{-# LANGUAGE OverloadedStrings, DeriveGeneric #-}

module JSONParserModule
    (parseMovies
    , parseActors
    , parsePages
    , parseCinemas
    , parseMovies2
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

-- ################################## Results #################################

data Results = Results {id :: Int, title :: T.Text, release_date :: T.Text}
  deriving (Show, Generic)

instance FromJSON Results where
   parseJSON (Object v) = Results <$> v .: "id" <*> v .: "title" <*> v .: "release_date"
   parseJSON _ = mzero

instance ToJSON Results where
   toJSON (Results id title release_date) = object ["id" .= id, "title" .= title, "release_date" .= release_date]

-- ################################## Movie ###################################

data MovieFromJSON = MovieFromJSON {results :: [Results], pages :: Int}
  deriving (Show, Generic)

instance FromJSON MovieFromJSON where
   parseJSON (Object v) = MovieFromJSON <$> v .: "results" <*> v .: "total_pages"
   parseJSON _ = mzero

instance ToJSON MovieFromJSON where
  toJSON (MovieFromJSON results pages) = object ["results" .= results, "total_pages" .= pages]

parsePages :: B.ByteString -> Int
parsePages b = pages fromJSON
   where fromJSON = fromJust $ decode b

instance FromJSON Movie where
   parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
   parseJSON _ = mzero

convertMovie :: Results -> Movie
convertMovie (Results i t r) = Movie i (T.unpack t) (T.unpack r)

parseMovies :: B.ByteString -> [Movie]
parseMovies b = map convertMovie moviesText
  where fromJSON = fromJust $ decode b
        moviesText = results fromJSON

-- ############################### Actors ######################################

data TmpActor = TmpActor Int String

instance FromJSON TmpActor where
    parseJSON (Object o) = TmpActor <$> o .: "id" <*> o .: "name"
    parseJSON _ = mzero

parseActors :: B.ByteString -> Movie -> [Actor]
parseActors cn movie = map (converteTmp movie) (fromJust $ parseMaybe actorParser =<< decode cn)
  where
    converteTmp :: Movie -> TmpActor -> Actor
    converteTmp movie (TmpActor aId name) = Actor aId name [movie]

actorParser :: Value -> Parser [TmpActor]
actorParser = withObject "actorParser" $ \o -> o.: "cast"

-- ################################# Movie2 ####################################
instance FromJSON Movie2 where
  parseJSON (Object o) = Movie2 <$>  o .: "title"
  parseJSON _ = mzero

parseMovies2 :: B.ByteString -> [Movie2]
parseMovies2 m = fromMaybe [] (parseMaybe movies2Parser =<< decode m)

movies2Parser :: Value -> Parser [Movie2]
movies2Parser = withObject "movies2Parser" $ \o -> o.: "listings"

-- #################################### Cinemas ###############################
instance FromJSON Cinema where
  parseJSON (Object o) = Cinema <$>  o .: "id" <*> o .: "name" <*> o .: "distance"
  parseJSON _ = mzero

parseCinemas :: B.ByteString -> [Cinema]
parseCinemas cn = fromJust $ parseMaybe cinemaParser =<< decode cn

cinemaParser :: Value -> Parser [Cinema]
cinemaParser = withObject "cinemaParser" $ \o -> o.: "cinemas"
