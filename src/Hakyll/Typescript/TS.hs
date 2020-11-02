-- |Compilers for compiling and compressing typescript resources.
-- There are various helper compilers for common compilation cases.
-- In particular, 'compressJtsCompiler' will compile both typescript
-- and javascript files, then compress them.
--
-- There are other variations that may be useful as well. 'compressTsCompiler'
-- is similar to 'compressJtsCompiler', but is more strict and will not accept
-- arbitrary javascript. 'compressTsCompilerWith' is the most general combinator,
-- using this you can pass arbitrary arguments to the typescript compiler prior
-- to compilation. These results are then minified as well.
--
-- Finally, if you just want to compile typescript without minification, perhaps
-- for readability, use any of the functions that are not prefixed with "compress".
-- Again, here 'tsCompilerWith' is the most general, allowing additional compiler
-- arguments to be passed prior to compilation. 'jtsCompiler' is provided for
-- convenience to support flexibly compiling javascript and typescript without
-- compression.
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
import           Hakyll.Core.Item
import           Hakyll.Core.Compiler
import           Hakyll.Process             (execName, execCompilerWith, ExecutableArg(..), CompilerOut(CStdOut))

import           Hakyll.Typescript.Internal

-- |Arguments to pass to the typescript compiler.
type TSArgs = [String]

-- |Compiles the typescript or javascript 'Hakyll.Core.Item.Item' to javascript.
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
tsCompilerWith args = execCompilerWith (execName "tsc") allArgs CStdOut
  where
  -- this won't work on Windows, but that's probably fine
  defaultArgs = [ProcArg "--outFile", ProcArg "/dev/stdout"]
  allArgs     = defaultArgs <> fmap ProcArg args <> [HakFilePath]
