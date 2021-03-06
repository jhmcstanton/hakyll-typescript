-- |Internal helper utilities. Subject to change. Avoid relying on anything
-- found in here.
module Hakyll.Typescript.Internal
  (
    minifyJs,
    withMinifyJs
  )
  where

import           Data.ByteString.Lazy (ByteString)
import           Hakyll (withItemBody, Compiler, Item)
import           Text.Jasmine

minifyJs :: ByteString -> Compiler ByteString
minifyJs = pure . minify

withMinifyJs :: Compiler (Item ByteString) -> Compiler (Item ByteString)
withMinifyJs m = m >>= withItemBody minifyJs
