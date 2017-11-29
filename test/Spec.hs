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
dataBaseModuleTests = TestList [ TestLabel "Testing if the result is nothing" dataBaseTest,
                                 TestLabel "Testing the database module" dataBaseTest2,
                                 TestLabel "More complex test of the database module" dataBaseTest3,
                                 TestLabel "Trying a inconsistent database" dataBaseTest4,
                                 TestLabel "Trying a inconsistent database" dataBaseTest5 ]

setupTest :: [Movie] -> [Actor] -> String -> String -> Maybe[Movie] -> Test
setupTest movie actor name description expected = TestCase ( do
  conn <- dbConnect
  _ <- initialiseDB conn
  _ <- insertMovieIntoDB conn movie
  _ <- insertActorIntoDB conn actor
  movie <- searchMoviesInDB conn name
  disconnectDB conn
  assertEqual description movie expected
  )

dataBaseTest :: Test
dataBaseTest = setupTest [] [] "test" "Shouldn fine a movie" Nothing


dataBaseTest2 :: Test
dataBaseTest2 = setupTest [Movie 1 "Movie"] [Actor 1 "Johannes" [1]]
                  "Johannes" "Should find a movie" (Just [Movie 1 "Movie"])

dataBaseTest3 :: Test
dataBaseTest3 = setupTest [ Movie 1 "Doctor Who", Movie 2 "Lord of the Rings", Movie 3 "Star Wars" ]
                  [ Actor 1 "Johannes" [1,2,3], Actor 2 "Manuel" [2,3], Actor 3 "Liam" [1] ]
                  "Johannes" "Should find tree a movies"
                  (Just [ Movie 1 "Doctor Who", Movie 2 "Lord of the Rings", Movie 3 "Star Wars" ])

dataBaseTest4 :: Test
dataBaseTest4 = setupTest [] [Actor 1 "Clara Oswald" [1]] "Clara Oswald" "Should return Nothig" Nothing

dataBaseTest5 :: Test
dataBaseTest5 = setupTest [Movie 1 "Doctor Who"] [] "Clara Oswald" "Should return Nothig" Nothing

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
