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
    tscErrorsReported,
    handlesUserArgs,
    compressedFileSizeSmaller,
    compressedFileSizeSmallerWithArgs,
    compressionErrorsReported,
    jtsCompilerWithCompilesJSWithArgs,
    jtsCompilerCompilesJs,
    jtsCompilerCompilesTs,
    compressJtsCompilerCompressesJs,
    compressJtsCompilerCompressesTs,
    compressJtsCompilerWithCompressesWithArgs,
    compressJtsCompilerWithReportsErrors
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

tscErrorsReported :: TestTree
tscErrorsReported = testCase "Compilation errors reported" $ do
  runTestCompiler helloWorldJS tsCompiler
                  "tsc compilation success (but it shouldn't have)"
                  expectFailure

handlesUserArgs :: TestTree
handlesUserArgs = testCase "Successfully passes user arguments to tsc" $ do
  runTestCompiler helloWorldJS (tsCompilerWith ["--allowJs", "true"])
                  "tsc compilation failure" expectSuccess

compressedFileSizeSmaller :: TestTree
compressedFileSizeSmaller = testCase "Valid TS minifies successfully" $ do
  runTestCompiler helloWorldTS compressTsCompiler
                  "tsc compilation failure" expectSuccess

compressedFileSizeSmallerWithArgs :: TestTree
compressedFileSizeSmallerWithArgs = testCase "Valid JS minifies successfully" $ do
  runTestCompiler helloWorldTS (compressTsCompilerWith ["--allowJs", "true"])
                  "tsc compilation failure" expectSuccess

compressionErrorsReported :: TestTree
compressionErrorsReported =
  testCase "Compilation errors in compress ts compiler are reported" $ do
    runTestCompiler helloWorldJS compressTsCompiler
                  "tsc compilation success" expectFailure

jtsCompilerWithCompilesJSWithArgs :: TestTree
jtsCompilerWithCompilesJSWithArgs =
  testCase "Compiles js and accepts more args" $ do
    runTestCompiler helloWorldJS (jtsCompilerWith ["--target", "ES6"])
                  "tsc compilation failed" expectSuccess

jtsCompilerCompilesJs :: TestTree
jtsCompilerCompilesJs = testCase "Compiles js successfully" $ do
    runTestCompiler helloWorldJS jtsCompiler
                  "tsc compilation failed" expectSuccess

jtsCompilerCompilesTs :: TestTree
jtsCompilerCompilesTs = testCase "Compiles ts successfully" $ do
    runTestCompiler helloWorldTS jtsCompiler
                  "tsc compilation failed" expectSuccess

compressJtsCompilerWithCompressesWithArgs :: TestTree
compressJtsCompilerWithCompressesWithArgs =
  testCase "Compresses and uses more args" $ do
    runTestCompiler helloWorldTS (compressJtsCompilerWith ["--target", "ES5"])
                  "tsc compilation failed" expectSuccess

compressJtsCompilerCompressesTs :: TestTree
compressJtsCompilerCompressesTs =
  testCase "Compresses ts" $ do
    runTestCompiler helloWorldTS compressJtsCompiler
                  "tsc compilation failed" expectSuccess

compressJtsCompilerCompressesJs :: TestTree
compressJtsCompilerCompressesJs =
  testCase "Compresses js" $ do
    runTestCompiler helloWorldJS compressJtsCompiler
                  "tsc compilation failed" expectSuccess

compressJtsCompilerWithReportsErrors :: TestTree
compressJtsCompilerWithReportsErrors =
  testCase "jts compiler reports errors" $ do
    runExceptionCompiler helloWorldTS
      (compressJtsCompilerWith ["--target", "ES2020"])
