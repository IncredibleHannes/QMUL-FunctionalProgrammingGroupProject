module DataStructures(
      Movie,
      Actor,
      Cinema
    ) where

data Movie = Movie Int String
data Actor = Actor Int String [Int]
data Cinema = Cinema Int String Int
