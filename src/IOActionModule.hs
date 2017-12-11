{- |
   Module     :IOActionModule
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

This module provides all functions that handles user interaction

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

askForLocation :: IO String
askForLocation = do
  putStrLn "Please enter your location: "
  getLine

askForActor :: IO String
askForActor = do
  putStrLn "Please enter the actore name you want to search for: "
  getLine

askToSelectMovie :: IO Int
askToSelectMovie = do
  putStrLn "Please selecte a movie now: "
  readLn

printCinemas :: [Cinema] -> IO ()
printCinemas c = do
  putStrLn "The following cinemas in your area show this film: "
  printCinemas' c
    where
      printCinemas' []     = return ()
      printCinemas' (x:xs) = do
        print x
        printCinemas' xs

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
