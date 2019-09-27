module Hakyll.Typescript.TS
  (
    compressJtsCompiler,
    compressJtsCompilerWith,
    compressTsCompiler,
    compressTsCompilerWith,
    jtsCompiler,
    jtsCompilerWith,
    tsCompiler,
    tsCompilerWith,
    TSArgs
  )
  where

import           Data.ByteString.Lazy.Char8 (ByteString)
import           GHC.Conc                   (atomically)
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler
import           System.Process.Typed

import           Hakyll.Typescript.Internal

type TSArgs = [String]

compressJtsCompiler :: Compiler (Item ByteString)
compressJtsCompiler = compressJtsCompilerWith mempty

-- |Compiles the typescript 'Hakyll.Core.Item.Item' to javascript, then
-- minifies the result.
compressTsCompiler :: Compiler (Item ByteString)
compressTsCompiler = compressTsCompilerWith mempty

-- |Compiles the typescript or javascript 'Hakyll.Core.Item.Item' to javascript,
-- then minifies the result
compressJtsCompilerWith :: TSArgs -> Compiler (Item ByteString)
compressJtsCompilerWith args = withMinifyJs $ jtsCompilerWith args

-- |Compiles the typescript 'Hakyll.Core.Item.Item' to javascript, then
-- minifies the result. Passes all typescript arguments to the typescript
-- compiler for compilation.
compressTsCompilerWith :: TSArgs -> Compiler (Item ByteString)
compressTsCompilerWith args = withMinifyJs $ tsCompilerWith args

-- |Compiles the typescript or javascript 'Hakyll.Core.Item.Item' to javascript.
jtsCompiler :: Compiler (Item ByteString)
jtsCompiler = jtsCompilerWith mempty

-- |Compiles the typescript 'Hakyll.Core.Item.Item' to javascript.
tsCompiler :: Compiler (Item ByteString)
tsCompiler = tsCompilerWith mempty

-- |Compiles the typescript or javascript 'Hakyll.Core.Item.Item' to javascript.
-- Passes the provided 'TSArgs' to the typescript compiler.
jtsCompilerWith :: TSArgs -> Compiler (Item ByteString)
jtsCompilerWith args = tsCompilerWith $ ["--allowJs", "true"] <> args

-- |Compiles the typescript 'Hakyll.Core.Item.Item' to javascript.
-- Passes the provided 'TSArgs' to the typescript compiler.
tsCompilerWith :: TSArgs -> Compiler (Item ByteString)
tsCompilerWith args = do
  -- this won't work on Windows, but that's probably fine
  let defaultArgs = ["--outFile", "/dev/stdout"]
  tsPath <- getResourceFilePath
  let fullArgs = defaultArgs <> args <> [tsPath]
  jsBody <- unsafeCompiler $ tsc fullArgs
  -- just using this to get at the item
  oldTsBody <- getResourceString
  pure $ itemSetBody jsBody oldTsBody

tsc :: TSArgs -> IO ByteString
tsc args = withProcess procConf waitJsOutput where
  procConf = setStdout byteStringOutput . proc "tsc" $ args
  waitJsOutput process = do
    let stmJs = getStdout process
    js <- atomically stmJs
    checkExitCode process
    pure js
