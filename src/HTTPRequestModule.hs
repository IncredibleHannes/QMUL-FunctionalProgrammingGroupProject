module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores
    ) where

import Data.List
import Control.Monad
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

movieReqURL :: String -> Int -> IO String
movieReqURL fromDate i = do
  date' <- getCurrentTime
  let date = toGregorian date'
  return $ concat ["https://api.themoviedb.org/3/discover/movie?api_key=",
                   "77a5749742a2117c0b9c739d7bad6518&language=en-US&sort_by=",
                   "popularity.desc&include_adult=false&include_video=false&page=",
                   show i, "&primary_release_date.gte=", fromDate,
                   "&primary_release_date.lte=", dShow $ getYr date, "-",
                   dShow $ getMth date, "-", dShow $ getDay date]
    where dShow x = if x < 10 then "0" ++ show x else show x

makeURI :: String -> URI
makeURI str = fromJust $ parseURI str

httpGetListOfMovies :: String -> IO [Movie]
httpGetListOfMovies fromDate = do
  pages <- fmap parsePages (N.simpleHttp =<< movieReqURL fromDate 1)
  requestList <- mapM (N.simpleHttp <=< movieReqURL fromDate) [1..pages]
  return $ concatMap parseMovies requestList

httpGetListOfActores = undefined
