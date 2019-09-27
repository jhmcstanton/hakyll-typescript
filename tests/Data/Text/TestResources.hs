{-# LANGUAGE PartialTypeSignatures #-}
module Data.Text.TestResources
  (
    bad,
    expectFailure,
    expectSuccess,
    helloWorldJS,
    helloWorldTS,
    resourceRoot,
    runTestCompiler
  )
  where

import           Hakyll
import           Hakyll.Core.Logger  (new, Verbosity(..))
import           Hakyll.Core.Runtime
import           System.Directory    (removeDirectoryRecursive)
import           System.Exit
import           Test.Tasty.HUnit    ((@?))

resourceRoot :: FilePath
resourceRoot = "tests/resources/"

helloWorldTS :: FilePath
helloWorldTS = resourceRoot <> "hello_world.ts"

helloWorldJS :: FilePath
helloWorldJS = resourceRoot <> "hello_world.js"

bad          :: FilePath
bad          = resourceRoot <> "bad.ts"

testTmpDir :: FilePath
testTmpDir = "_test/"

testConfiguration :: Configuration
testConfiguration = defaultConfiguration
  {
    destinationDirectory = testTmpDir <> "site"
  , storeDirectory       = testTmpDir <> "store"
  , tmpDirectory         = testTmpDir <> "tmp"
  , providerDirectory    = "."
  }

runTestCompiler :: _
                => FilePath
                -> Compiler (Item a)
                -> String -- assertion failure message
                -> (ExitCode -> IO Bool) -- assertion. Should not throw!
                -> IO ()
runTestCompiler testResource testCompiler msg assertion = do
  logger <- new Error
  (exitCode, _) <- run testConfiguration logger $ do
    match (fromGlob testResource) $ do
      route $ setExtension "js"
      compile testCompiler
  assertResult <- assertion exitCode
  removeDirectoryRecursive testTmpDir
  assertResult @? msg

expectSuccess :: ExitCode -> IO Bool
expectSuccess = pure . (== ExitSuccess)

expectFailure :: ExitCode -> IO Bool
expectFailure = pure . (== ExitFailure 1)
