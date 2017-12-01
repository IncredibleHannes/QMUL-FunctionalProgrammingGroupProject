{- |
   Module     : Tests
   Copyright  : Copyright (C) 2017 Johannes Hartmann
   License    : MIT

   Maintainer : Johannes Hartmann <ec17512@qmul.ac.uk>
   Stability  : provisional
   Portability: portable

Some tests to test the application

Written by Johannes Hartmann, ec17512@qmul.ac.uk
-}

import Test.HUnit

import DataBaseModule
import DataStructures

main :: IO Counts
main = do
  _ <- runTestTT dataBaseModuleTests
  _ <- runTestTT dataStructuresTests
  _ <- runTestTT httpRequestModuleTests
  _ <- runTestTT httpRequestModule2Tests
  _ <- runTestTT ioActionModuleTests
  runTestTT jsonParserModuleTests


dataBaseModuleTests :: Test
dataBaseModuleTests = TestList [ TestLabel "Testing if the result is nothing"
                        dataBaseTest,
                        TestLabel "Testing the database module" dataBaseTest2,
                        TestLabel "More complex test of the database module"
                        dataBaseTest3,
                        TestLabel "Trying a inconsistent database" dataBaseTest4,
                        TestLabel "Trying a inconsistent database" dataBaseTest5,
                        TestLabel "Cleaning up database 1" dataBaseCleanupTest,
                        TestLabel "Cleaning up database 2" dataBaseCleanupTest1,
                        TestLabel "Cleaning up database 3" dataBaseCleanupTest2,
                        TestLabel "Cleaning up database 4" dataBaseCleanupTest3,
                        TestLabel "Cleaning up database 5" dataBaseCleanupTest4]

setupTest :: [Movie] -> [Actor] -> String -> String -> Maybe[Movie] -> Test
setupTest movie actor name description expected = TestCase ( do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn movie
  insertActorIntoDB conn actor
  movie <- searchMoviesInDB conn name
  clearDatabase conn
  disconnectDB conn
  assertEqual description expected movie
  )

dataBaseTest :: Test
dataBaseTest = setupTest [] [] "test" "Shouldn fine a movie" Nothing


dataBaseTest2 :: Test
dataBaseTest2 = setupTest [Movie 1 "Movie" "2017-07-30"] [Actor 1 "Johannes"
                  [Movie 1 "Movie" "2017-07-30"] ]
                  "Johannes" "Should find a movie" (Just
                  [Movie 1 "Movie" "2017-07-30"])

dataBaseTest3 :: Test
dataBaseTest3 = setupTest [ Movie 1 "Doctor Who" "2017-07-30",
                            Movie 2 "Lord of the Rings" "2017-07-30",
                            Movie 3 "Star Wars" "2017-07-30" ]
                  [ Actor 1 "Johannes" [ Movie 1 "Doctor Who" "2017-07-30",
                  Movie 2 "Lord of the Rings" "2017-07-30",
                  Movie 3 "Star Wars" "2017-07-30" ],
                    Actor 2 "Manuel" [ Movie 1 "Doctor Who" "2017-07-30"],
                    Actor 3 "Liam" [] ]
                  "Johannes" "Should find tree a movies"
                  (Just [ Movie 1 "Doctor Who" "2017-07-30",
                          Movie 2 "Lord of the Rings" "2017-07-30",
                          Movie 3 "Star Wars" "2017-07-30" ])

dataBaseTest4 :: Test
dataBaseTest4 = setupTest [] [Actor 1 "Clara Oswald" [Movie 1 "Doctor Who" "2017-07-30"]]
                  "Clara Oswald" "Should return Nothig" Nothing

dataBaseTest5 :: Test
dataBaseTest5 = setupTest [Movie 1 "Doctor Who" "2017-07-30"] []
                  "Clara Oswald" "Should return Nothig" Nothing

setupCleanupTest :: [Movie] -> String -> [Movie] -> String -> Test
setupCleanupTest movies date expected description = TestCase ( do
  conn1 <- dbConnect
  initialiseDB conn1
  insertMovieIntoDB conn1 movies
  cleanupDatabase conn1 date
  movies <- getMoviesFromDatabase conn1
  clearDatabase conn1
  disconnectDB conn1
  assertEqual description expected movies
  )

dataBaseCleanupTest :: Test
dataBaseCleanupTest = setupCleanupTest [Movie 1 "TestMovie" "2016-01-01"]
                        "2017-01-01" [] "Should not find any movies anymore"

dataBaseCleanupTest1 :: Test
dataBaseCleanupTest1 = setupCleanupTest [Movie 1 "TestMovie" "2016-01-01",
                        Movie 2 "TestMovie" "2017-01-02"]
                        "2017-01-01" [Movie 2 "TestMovie" "2017-01-02"]
                        "Should find only movie 2"

dataBaseCleanupTest2 :: Test
dataBaseCleanupTest2 = setupCleanupTest [] "2017-01-01" [] "Should find any movie"

setupCleanupTest2 :: [Movie] -> [Actor] -> String -> String -> [Actor] -> Test
setupCleanupTest2 movies actores date description expected = TestCase ( do
  conn <- dbConnect
  initialiseDB conn
  insertMovieIntoDB conn movies
  insertActorIntoDB conn actores
  cleanupDatabase conn date
  actores <- getActorsFromDatabase conn
  clearDatabase conn
  disconnectDB conn
  assertEqual description actores expected
  )

dataBaseCleanupTest3 :: Test
dataBaseCleanupTest3 = setupCleanupTest2 [Movie 1 "Doctor Who" "2016-01-01",
                        Movie 2 "Boradchurch" "2017-01-02"]
                        [Actor 1 "David Tennant" [Movie 1 "Doctor Who" "2016-01-01",
                        Movie 2 "Boradchurch" "2017-01-02"]]
                        "2017-01-03" "The actore should be removed" []
dataBaseCleanupTest4 :: Test
dataBaseCleanupTest4 = setupCleanupTest2 [Movie 1 "Doctor Who" "2016-01-01",
                        Movie 2 "Boradchurch" "2017-01-02"]
                        [Actor 1 "David Tennant" [Movie 1 "Doctor Who" "2016-01-01",
                         Movie 2 "Boradchurch" "2017-01-02"]]
                        "2016-02-01" "The movie schould be removed from the actore"
                        [Actor 1 "David Tennant" [Movie 2 "Boradchurch" "2017-01-02"]]

dataStructuresTests :: Test
dataStructuresTests     = TestList []
httpRequestModuleTests :: Test
httpRequestModuleTests  = TestList []
httpRequestModule2Tests :: Test
httpRequestModule2Tests = TestList []
ioActionModuleTests :: Test
ioActionModuleTests     = TestList []
jsonParserModuleTests :: Test
jsonParserModuleTests   = TestList []
