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
askToSelectMovie = undefined{-do
  putStrLn "Please selecte a movie now: "
  number <- getLine
  return (number :: Int)-}

printCinemas :: [Cinema] -> IO ()
printCinemas = undefined

printMovies :: Maybe [Movie] -> IO()
printMovies Nothing  = error "Could't find a movie for the given actore"
printMovies (Just x) = do
  putStrLn "The given actor plays in the following movies"
  printMoviesHelper x 1
    where
      printMoviesHelper :: [Movie] -> Int -> IO()
      printMoviesHelper [] _ = return ()
      printMoviesHelper (x:xs) i = do
        putStrLn ("(" ++ show i ++ ") " ++ show x)
        printMoviesHelper xs (i + 1)
