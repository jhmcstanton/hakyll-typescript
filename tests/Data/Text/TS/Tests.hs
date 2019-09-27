module Data.Text.TS.Tests
  (
    tests
  )
  where

import           Data.Text.TS
import           Data.Text.TestResources

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Data.Text.TS.Tests"
  [
    compilesTs,
    errorsOnBadTs,
    errorsOnJsByDefault,
    handlesUserArgs
  ]

compilesTs :: TestTree
compilesTs = testCase "Compiles Valid TS" $ do
  runTestCompiler helloWorldTS tsCompiler
                  "tsc compiler error" expectSuccess

errorsOnBadTs :: TestTree
errorsOnBadTs = testCase "Raises an exception on invalid TS" $ do
  runTestCompiler bad tsCompiler
                  "tsc compilation success (but it shouldn't have)"
                  expectFailure

errorsOnJsByDefault :: TestTree
errorsOnJsByDefault = testCase "JS is not supported by default" $ do
  runTestCompiler helloWorldJS tsCompiler
                  "tsc compilation success (but it shouldn't have)"
                  expectFailure

handlesUserArgs :: TestTree
handlesUserArgs = testCase "Successfully passes user arguments to tsc" $ do
  runTestCompiler helloWorldJS (tsCompilerWith ["--allowJs", "true"])
                  "tsc compilation failure" expectSuccess
