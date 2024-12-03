{-# LANGUAGE ImportQualifiedPost #-}
import Test.Tasty ( TestTree, defaultMain, testGroup )
import Test.Tasty.HUnit ( testCase, (@?=) )
import Test.Tasty.QuickCheck as QC

import Lib1 qualified
import Lib2 qualified
import Lib3 (Statements, renderStatements, parseStatements, genStatements)

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [unitTests, propertyTests]

unitTests :: TestTree
unitTests = testGroup "Lib1 tests"
  [ testCase "List of completions is not empty" $
      null Lib1.completions @?= False,
    testCase "Parsing case 1 - give a better name" $
      Lib2.parseQuery "" @?= (Left "Some error message"),
    testCase "Parsing case 2 - give a better name" $
      Lib2.parseQuery "o" @?= (Left "Some error message")
  ]

instance Arbitrary Lib3.Statements where
  arbitrary = Lib3.genStatements

prop_renderParse :: Statements -> Property
prop_renderParse statements =
  let rendered = renderStatements statements
      parsed = parseStatements rendered
   in counterexample ("Rendered: " ++ rendered ++ "\nParsed: " ++ show parsed) $
        parsed == Right (statements, "")

propertyTests :: TestTree
propertyTests = testGroup "Lib3 property tests"
  [
    QC.testProperty "Render and Parse" prop_renderParse
  ]