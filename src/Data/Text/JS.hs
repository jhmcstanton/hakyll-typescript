module Data.Text.JS
  (
    compressJsCompiler
  )
  where

import           Hakyll
import           Data.ByteString.Lazy.Char8 (pack, unpack)
import           Text.Jasmine

compressJsCompiler :: Compiler (Item String)
compressJsCompiler = getResourceString >>= withItemBody minifyJs
  where
    minifyJs = pure . unpack . minify . pack
