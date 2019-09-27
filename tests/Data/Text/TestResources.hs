{-# LANGUAGE PartialTypeSignatures #-}
module Data.Text.TestResources
  (
    bad,
    expectFailure,
    expectSuccess,
    fileSize,
    helloWorldJS,
    helloWorldTS,
    outputSmaller,
    resourceRoot,
    runExceptionCompiler,
    runTestCompiler
  )
  where

import           Control.Exception   (handle, SomeException)
import           Hakyll
import           Hakyll.Core.Logger  (new, Verbosity(..))
import           Hakyll.Core.Runtime
import           System.Directory    (removeDirectoryRecursive)
import           System.Exit
import           System.IO
import           Test.Tasty.HUnit    ((@?), assertFailure)

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

testSiteDir :: FilePath
testSiteDir = testTmpDir <> "site/"

testToSitePath :: FilePath -> FilePath
testToSitePath p = testSiteDir <> name <> ".js" where
  name = reverse . drop 3 . reverse $ p

testConfiguration :: Configuration
testConfiguration = defaultConfiguration
  {
    destinationDirectory = testSiteDir
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
  logger        <- new Error
  (exitCode, _) <- run testConfiguration logger $ do
    match (fromGlob testResource) $ do
      route $ setExtension "js"
      compile testCompiler
  assertResult  <- assertion exitCode
  removeDirectoryRecursive testTmpDir
  assertResult @? msg

runExceptionCompiler :: _ => FilePath -> Compiler (Item a) -> IO ()
runExceptionCompiler testResource testCompiler = do
  logger <- new Error
  result <- handle (\e -> printException e >> pure True) $ do
    _ <- run testConfiguration logger $ do
      match (fromGlob testResource) $ do
        route $ setExtension "js"
        compile testCompiler
    pure False
  removeDirectoryRecursive testTmpDir
  result @? "Expected exception."

expectSuccess :: ExitCode -> IO Bool
expectSuccess = pure . (== ExitSuccess)

expectFailure :: ExitCode -> IO Bool
expectFailure = pure . (== ExitFailure 1)

printException :: SomeException -> IO ()
printException e = putStrLn $ "Caught exception: " ++ show e

fileSize :: FilePath -> IO Integer
fileSize p = withFile p ReadMode hFileSize

outputSmaller :: FilePath -> ExitCode -> IO Bool
outputSmaller testPath exitCode = do
  origSize <- fileSize testPath
  siteSize <- fileSize $ testToSitePath testPath
  pure $ exitCode == ExitSuccess && siteSize < origSize
