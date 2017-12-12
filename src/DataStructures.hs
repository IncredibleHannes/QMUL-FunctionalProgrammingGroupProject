{- |
   Module     : DataStructures
   Copyright  : Copyright (C) 2017 Johannes Hartmann, Liam Kelly, Manuel Campos Villarreal
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : stable
   Portability: portable

Datatypes used in the application.

Written by Johannes Hartmann, Liam Kelly, Manuel Campos Villarreal
-}
module DataStructures(
      Movie(Movie),
      Actor(Actor),
      Cinema(Cinema),
      Movie2(Movie2)
    ) where

{- | Data structure representig a movie. The first parameter is the ID and the
     second is the name of the movie and the third the release day -}
data Movie = Movie { movieId :: Int, title :: String, releaseDate :: String }
  deriving (Eq)

instance Show Movie where
  show (Movie _ title _) = title

{- | Data structure representing a Movie from the second API, whicht have no cinemaId and
     release date. -}
newtype Movie2 = Movie2 { movieTitle :: String }
  deriving (Eq, Show)

{- | Data structure representig a actor. The first parameter is the ID, the
     second is the name of the actor and the third are all movies he plays in -}
     -- maybe use list of mio
data Actor = Actor { actorId :: Int, actorName :: String, movie :: [Movie]}
  deriving (Eq, Show)

{- | Data structure representig a cinema. The first parameter is the ID, the
     second is the name of the cinema and the third is the range to the given
     location -}
data Cinema = Cinema { cinemaId :: String, cinemaName :: String, distance :: Float }
  deriving (Eq)

instance Show Cinema where
  show (Cinema _ title distance) = "Cinema: \"" ++  title ++ "\" Distance: " ++ show distance
