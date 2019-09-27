module Data.Text.TS
  (
    tsCompiler,
    tsCompilerWith,
    TSArgs
  )
  where

import           Data.ByteString.Lazy.Char8 (unpack)
import           GHC.Conc                   (atomically)
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler

import           System.Process.Typed

type TSArgs = [String]

-- |Compiles the typescript `Hakyll.Core.Item.Item` to javascript.
tsCompiler :: Compiler (Item String)
tsCompiler = tsCompilerWith mempty

-- |Compiles the typescript `Hakyll.Core.Item.Item` to javascript.
-- Passes the provided `TSArgs` to the typescript compiler.
tsCompilerWith :: TSArgs -> Compiler (Item String)
tsCompilerWith args = do
  -- this won't work on Windows, but that's probably fine
  let defaultArgs = ["--outFile", "/dev/stdout"]
  tsPath <- getResourceFilePath
  let fullArgs = defaultArgs <> args <> [tsPath]
  jsBody <- unsafeCompiler $ tsc fullArgs
  -- just using this to get at the item
  oldTsBody <- getResourceString
  pure $ itemSetBody jsBody oldTsBody

tsc :: TSArgs -> IO String
tsc args = withProcess procConf waitJsOutput where
  procConf = setStdout byteStringOutput . proc "tsc" $ args
  waitJsOutput process = do
    let stmJs = getStdout process
    js <- atomically stmJs
    checkExitCode process
    pure . unpack $ js
