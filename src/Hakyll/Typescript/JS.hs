-- |Exports 'compressJsCompiler', which simply minifies
-- javascript files (but not typescript). This is strictly less
-- general than the functions found in "Hakyll.Typescript.TS", but
-- is useful when you want to skip over running the typescript compiler.
module Hakyll.Typescript.JS
  (
    compressJsCompiler
  )
  where

import           Data.ByteString.Lazy
import           Hakyll.Typescript.Internal
import           Hakyll

-- |Minifies the javascript 'Hakyll.Core.Item.Item'.
compressJsCompiler :: Compiler (Item ByteString)
compressJsCompiler = withMinifyJs getResourceLBS
