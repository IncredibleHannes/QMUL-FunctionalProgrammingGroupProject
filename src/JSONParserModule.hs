{- |
   Module     : JSONParserModule
   Copyright  : Copyright (C) 2017  Liam Kelly, Manuel Campos Villarreal, Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: stable

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

data Results = Results {id :: Int, title :: String, releaseDate :: String}
  deriving (Show, Generic)

instance FromJSON Results where
   parseJSON (Object v) = Results <$> v .: "id" <*> v .: "title" <*> v .: "release_date"
   parseJSON _ = mzero

-- ################################## Movie ###################################

data MovieFromJSON = MovieFromJSON {results :: [Results], pages :: Int}
  deriving (Show, Generic)

instance FromJSON MovieFromJSON where
   parseJSON (Object v) = MovieFromJSON <$> v .: "results" <*> v .: "total_pages"
   parseJSON _ = mzero

parsePages :: B.ByteString -> Int
parsePages b = pages (fromJust $ decode b)

instance FromJSON Movie where
   parseJSON (Object o) = Movie <$> o .: "id" <*> o .: "title" <*> o .: "release_date"
   parseJSON _ = mzero

convertMovie :: Results -> Movie
convertMovie (Results i t r) = Movie i t r

-- | This function parses a given byte string representig JSON into a list of movies
parseMovies :: B.ByteString -> [Movie]
parseMovies b = map convertMovie moviesText
  where fromJSON = fromJust $ decode b
        moviesText = results fromJSON

-- ############################### Actors ######################################

data TmpActor = TmpActor Int String

instance FromJSON TmpActor where
    parseJSON (Object o) = TmpActor <$> o .: "id" <*> o .: "name"
    parseJSON _ = mzero

{- | This function parses a given byte string representig JSON and a movie into a list
     actores -}
parseActors :: B.ByteString -> Movie -> [Actor]
parseActors cn movie = map (converteTmp movie) (fromMaybe [] (parseMaybe actorParser =<< decode cn))
  where
    converteTmp :: Movie -> TmpActor -> Actor
    converteTmp movie (TmpActor aId name) = Actor aId name [movie]

actorParser :: Value -> Parser [TmpActor]
actorParser = withObject "actorParser" $ \o -> o.: "cast"

-- ################################# Movie2 ####################################
instance FromJSON Movie2 where
  parseJSON (Object o) = Movie2 <$>  o .: "title"
  parseJSON _ = mzero

-- | This function parses a given byte string representig JSON into a list of movies2
parseMovies2 :: B.ByteString -> [Movie2]
parseMovies2 m = fromMaybe [] (parseMaybe movies2Parser =<< decode m)

movies2Parser :: Value -> Parser [Movie2]
movies2Parser = withObject "movies2Parser" $ \o -> o.: "listings"

-- #################################### Cinemas ###############################
instance FromJSON Cinema where
  parseJSON (Object o) = Cinema <$>  o .: "id" <*> o .: "name" <*> o .: "distance"
  parseJSON _ = mzero

-- | This function parses a given byte string representig JSON into a list of cinema
parseCinemas :: B.ByteString -> [Cinema]
parseCinemas cn = fromMaybe [] (parseMaybe cinemaParser =<< decode cn)

cinemaParser :: Value -> Parser [Cinema]
cinemaParser = withObject "cinemaParser" $ \o -> o.: "cinemas"
