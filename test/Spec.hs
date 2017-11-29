import Test.HUnit

main :: IO Counts
main = do
  _ <- runTestTT dataBaseModuleTests
  _ <- runTestTT dataStructuresTests
  _ <- runTestTT httpRequestModuleTests
  _ <- runTestTT httpRequestModule2Tests
  _ <- runTestTT ioActionModuleTests
  runTestTT jsonParserModuleTests

test1 :: Test
test1 = TestCase (assertEqual "one equals one" 1 1)

dataBaseModuleTests :: Test
dataBaseModuleTests     = TestList [TestLabel "Easy Test" test1]
dataStructuresTests :: Test
dataStructuresTests     = TestList []
httpRequestModuleTests :: Test
httpRequestModuleTests  = TestList []
httpRequestModule2Tests :: Test
httpRequestModule2Tests = TestList []
ioActionModuleTests :: Test
ioActionModuleTests     = TestList 
jsonParserModuleTests :: Test
jsonParserModuleTests   = TestList []
