module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores,
      download,
      movieReqURL
    ) where

import Data.List
import DataStructures
import JSONParserModule

import Network.URI
import Network.HTTP.Simple
import Network.HTTP.Client
import Data.DateTime
import Data.Maybe
import Data.Either

getYr :: (Integer, Int, Int, Int, Int, Int) -> Integer
getYr (yr, _, _, _, _, _) = yr

getMth :: (Integer, Int, Int, Int, Int, Int) -> Int
getMth (_, mth, _, _, _, _) = mth

getDay :: (Integer, Int, Int, Int, Int, Int) -> Int
getDay (_, _, day, _, _, _) = day

yrMinusThree :: Integer -> Integer
yrMinusThree x = if x <4 then x - 1 else x

mthMinusThree :: Int -> Int
mthMinusThree x 
  | x==1 = 10
  | x==2 = 11
  | x==3 = 12
  | x>=4 = x - 3

movieReqURL :: IO String
movieReqURL = do
  date' <- getCurrentTime
  let date = toGregorian date'
  let gteYr = yrMinus $ getYr date
  let gteMth = mthMinusThree $ getMth date
  let day = getDay date
  let mth = getMth date
  let yr = getYr date
  return $ concat ["https://api.themoviedb.org/3/discover/movie?api_key=77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=popularity.desc&include_adult=false&include_video=false&page=1&primary_release_date.gte=", dShow gteYr, "-" , dShow gteMth, "-", dShow day, "&primary_release_date.lte=", dShow yr, "-", dShow mth, "-", dShow day]
    where yrMinus x = if x<4 then x-1 else x
          dShow x = if x < 10 then "0" ++ show x else show x



actorReqURL :: String
actorReqURL = undefined

makeURI :: String -> URI
makeURI str = fromJust $ parseURI $ str

download :: String -> IO String
download str = do
  req <- parseRequest str
  resp <- httpJSON req
  let rBody = responseBody $ resp
  return rBody

httpGetListOfMovies :: IO [Movie]
httpGetListOfMovies = do
  url <- movieReqURL
  moviesStr <- download url
  let movies = parseMovies moviesStr
  return movies

httpGetListOfActores :: IO [Actor]
httpGetListOfActores = do
  actorsStr <- download actorReqURL
  let actors = parseActors actorsStr
  return actors
