module Data.Text.JS
  (
    compressJsCompiler
  )
  where

import           Data.ByteString.Lazy
import           Data.Text.Internal
import           Hakyll

-- |Minifies the javascript 'Hakyll.Core.Item.Item'.
compressJsCompiler :: Compiler (Item ByteString)
compressJsCompiler = withMinifyJs getResourceLBS
