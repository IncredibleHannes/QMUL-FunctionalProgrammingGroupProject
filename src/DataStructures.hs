{- |
   Module     : DataStructures
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

Datatypes used in the application.

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}
module DataStructures(
      Movie(Movie),
      Actor(Actor),
      Cinema(Cinema),
      Listings(Listings)
    ) where


{- | Data structure representig a movie. The first parameter is the ID and the
     second is the name of the movie and the third the release day -}
data Movie = Movie Int String String
  deriving (Eq, Show)
{- | Data structure representig a actor. The first parameter is the ID, the
     second is the name of the actor and the third are all movies he plays in -}
     -- maybe use list of mio
data Actor = Actor Int String [Movie]
  deriving (Eq, Show)
{- | Data structure representig a cinema. The first parameter is the ID, the
     second is the name of the cinema and the third is the range to the given
     location -}
data Cinema = Cinema String String Float
  deriving (Eq, Show)

data Listings = Listings String
  deriving (Eq, Show)
