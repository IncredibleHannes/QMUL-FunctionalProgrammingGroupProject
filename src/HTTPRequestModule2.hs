{- |
   Module     : HTTPRequestModule2
   Copyright  : Copyright (C) 2017 Manuel Campos Villarreal, Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This module provides all neccesary methodes to look up a all cinemas that plays
a given movie in a given area

Written by  Manuel Campos Villarreal, Johannes Hartmann
-}
module HTTPRequestModule2
    ( httpGetCinemaList,
      httpApiCinemaRequest
    ) where

import DataStructures
import JSONParserModule
import qualified Data.ByteString.Lazy as B
import Network.HTTP.Conduit (simpleHttp, HttpException)
import Data.List
import Data.Char
import Control.Monad
import Control.Exception

-- | for a given location this function returns a list of cinemas that are nearby
httpGetCinemaList :: String -> IO [Cinema]
httpGetCinemaList loc = fmap parseCinemas (simpleHttp $ cinemaReqURL loc)

cinemaReqURL:: String -> String
cinemaReqURL = (++) "https://api.cinelist.co.uk/search/cinemas/location/"

{- | for a given movie and a list of cinemas this function returns a list of
     cinemas that plays this movie -}
httpApiCinemaRequest :: Movie -> [Cinema] -> IO [Cinema]
httpApiCinemaRequest movie = filterM (playsMovie movie)

cinemaMoviesURL:: String -> String
cinemaMoviesURL = (++) "https://api.cinelist.co.uk/get/times/cinema/"

httpGetListOfMoives :: Cinema -> IO [Movie2]
httpGetListOfMoives (Cinema i _ _) = do
  let cinemaHandle = (\e -> return B.empty) :: HttpException -> IO B.ByteString
  fmap parseMovies2 (handle cinemaHandle (simpleHttp $ cinemaMoviesURL i))

playsMovie :: Movie -> Cinema -> IO Bool
playsMovie movie cinema = do
  movieList <- httpGetListOfMoives cinema
  return $ contains' movieList movie
    where
      contains' :: [Movie2] -> Movie -> Bool
      contains' [] _ = False
      contains' (Movie2 t1 : xs) m@(Movie _ t2 _) = isInfixOf (map toUpper t1)
                                                    (map toUpper t2)
                                                    || contains' xs m
