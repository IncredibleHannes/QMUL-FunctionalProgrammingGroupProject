module HTTPRequestModule
    ( httpGetListOfMovies,
      httpGetListOfActores,
      concatActors
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
import Control.Parallel.Strategies

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


-- | returns all Movies starting from a given time string
httpGetListOfMovies :: String -> IO [Movie]
httpGetListOfMovies fromDate = do
  pages <- fmap parsePages (N.simpleHttp =<< movieReqURL fromDate 1)
  requestList <- sequence $ parMap rseq (N.simpleHttp <=< movieReqURL fromDate) [1..pages]
  return $ concatMap parseMovies requestList

-- ############################### Actores #####################################

-- | returns a List of all actors playing in the given list of movies
httpGetListOfActores :: [Movie] -> IO [Actor]
httpGetListOfActores movies = do
  actores <- sequence $ parMap rseq getActores movies
  return $ concatActors actores


getActores :: Movie -> IO [Actor]
getActores m@(Movie movieId _ _) = do
  actoreJSON <- N.simpleHttp (actorReqUrl movieId)
  return $ parseActors actoreJSON m

actorReqUrl :: Int -> String
actorReqUrl aId = concat [ "https://api.themoviedb.org/3/movie/", show aId,
                          "/credits?api_key=77a5749742a2117c0b9c739d7bad6518" ]


{- | hepler function that concatitates the actores so that duplicates with different
     movies are combined together -}
concatActors :: [[Actor]] -> [Actor]
concatActors x = removeDups $ concat x
  where
    removeDups :: [Actor] -> [Actor]
    removeDups [] = []
    removeDups (x:xs) = let dups = getDups x (x:xs) in concatA (fst dups) : removeDups (snd dups)

    getDups :: Actor -> [Actor] -> ([Actor], [Actor])
    getDups _ [] = ([], [])
    getDups a1@(Actor _ n1 _) (a2@(Actor _ n2 _) : xs) = if n1 == n2
                                                          then (a2 : fst (getDups a1 xs), snd $ getDups a1 xs)
                                                          else (fst $ getDups a1 xs, a2 : snd (getDups a1 xs))
    concatA :: [Actor] -> Actor
    concatA [a] = a
    concatA (Actor aId1 n1 m1 : Actor aId2 n2 m2 : xs ) = concatA (Actor aId1 n1 (m1 ++ m2) : xs)
