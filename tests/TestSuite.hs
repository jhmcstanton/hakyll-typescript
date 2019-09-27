module Main where

import qualified Hakyll.Typescript.JS.Tests as JS
import qualified Hakyll.Typescript.TS.Tests as TS
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "hakyll-typescript"
  [
    JS.tests
  , TS.tests
  ]
