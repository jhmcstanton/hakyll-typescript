module Main where

import qualified Data.Text.TS.Tests as TS
import           Test.Tasty (defaultMain, testGroup)

main :: IO ()
main = defaultMain $ testGroup "hakyll-typescript"
  [
    TS.tests
  ]
