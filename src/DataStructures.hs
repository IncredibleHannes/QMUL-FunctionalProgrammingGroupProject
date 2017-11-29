module DataStructures(
      Movie(Movie),
      Actor(Actor),
      Cinema(Cinema)
    ) where

data Movie = Movie Int String
data Actor = Actor Int String [Int]
data Cinema = Cinema Int String Int
