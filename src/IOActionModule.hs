{- |
   Module     :IOActionModule
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: stable

This module provides all the necessary functions for handling the user interaction

Written by Johannes Hartmann
-}

module IOActionModule
    ( askForLocation,
      askForActor,
      askToSelectMovie,
      printCinemas,
      printMovies,
    ) where

import DataStructures

{- | This function asks the user for a location and returns it as a string -}
askForLocation :: IO String
askForLocation = do
  putStrLn "Please enter your location: "
  getLine

{- | This function asks the user for an actor and returns it as a string -}
askForActor :: IO String
askForActor = do
  putStrLn "Please enter the actor name you want to search for: "
  getLine

{- | This function asks the user to select a movie and returns it as an int -}
askToSelectMovie :: IO Int
askToSelectMovie = do
  putStrLn "Please select a movie from the List: "
  readLn

{- | This function prints a given list of cinemas to std out -}
printCinemas :: [Cinema] -> IO ()
printCinemas c = do
  putStrLn "the following cinemas in your area are currently showing the film: "
  printCinemas' c
    where
      printCinemas' []     = return ()
      printCinemas' (x:xs) = do
        print x
        printCinemas' xs

{- | This function prints a given list of movies to std out -}
printMovies :: [Movie] -> IO()
printMovies x = do
  putStrLn "The given actor plays in the following movies"
  printMoviesHelper x 1
    where
      printMoviesHelper :: [Movie] -> Int -> IO()
      printMoviesHelper [] _ = return ()
      printMoviesHelper (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show x)
        printMoviesHelper xs (i + 1)
