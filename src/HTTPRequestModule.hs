module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores,
      movieReqURL
    ) where

import Data.List
import DataStructures
import JSONParserModule

import qualified Network.HTTP.Conduit as N
import Data.DateTime
import Network.URI
import Data.Maybe
import Data.Either
import qualified Data.ByteString.Lazy as B

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

movieReqURL :: Int -> IO String
movieReqURL i = do
  date' <- getCurrentTime
  let date = toGregorian date'
  let gteYr = yrMinus (getYr date) (getMth date)
  let gteMth = mthMinusThree $ getMth date
  let day = getDay date
  let mth = getMth date
  let yr = getYr date
  return $ concat ["https://api.themoviedb.org/3/discover/movie?api_key=77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=popularity.desc&include_adult=false&include_video=false&page=", show i, "&primary_release_date.gte=", dShow gteYr, "-" , dShow gteMth, "-", dShow day, "&primary_release_date.lte=", dShow yr, "-", dShow mth, "-", dShow day]
    where yrMinus yr mth = if mth<4 then yr-1 else yr
          dShow x = if x < 10 then "0" ++ show x else show x

actorReqURL :: String
actorReqURL = undefined

makeURI :: String -> URI
makeURI str = fromJust $ parseURI $ str

httpGetListOfMovies :: IO [Movie]
httpGetListOfMovies = do
  url <- movieReqURL 1
  moviesStr <- N.simpleHttp url
  let pages = parsePages moviesStr
  let pageList = [1..pages]
  let movies = parseMovies moviesStr
  return movies

httpGetListOfActores :: IO [Actor]
httpGetListOfActores = undefined
