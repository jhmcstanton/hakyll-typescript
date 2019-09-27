module Hakyll.Typescript.JS.Tests
  (
    tests
  )
  where

import           Hakyll.Typescript.JS
import           Hakyll.Typescript.TestResources

import           Test.Tasty
import           Test.Tasty.HUnit

tests :: TestTree
tests = testGroup "Hakyll.Typescript.JS.Tests"
  [
    fileSizeSmaller,
    errorsOnBadJs
  ]

fileSizeSmaller :: TestTree
fileSizeSmaller = testCase "Valid js output is smaller than input" $
  runTestCompiler helloWorldJS compressJsCompiler
                  "Compression failed" (outputSmaller helloWorldJS)

errorsOnBadJs :: TestTree
errorsOnBadJs = testCase "Raises an exception when Js is invalid" $
  runExceptionCompiler bad compressJsCompiler
