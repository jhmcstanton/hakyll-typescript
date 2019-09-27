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
