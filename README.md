# hakyll-typescript

![Hackage Badge](https://img.shields.io/hackage/v/hakyll-typescript)

A simple [`hakyll`](http://hackage.haskell.org/package/hakyll)
typescript compiler for typescript and javascript resources. Uses
[`hjsmin`](http://hackage.haskell.org/package/hjsmin) for compression.

Assumes that the typescript compiler is installed and available to the
user running `hakyll`.

## Example Usage

In your hakyll site simply import the compiler you are interested in using
and provide it as the compiler for your relevant script files.

```haskell
import Hakyll.Typescript.TS

main = hakyll $ do
  -- Matches any file inside a the directory ./scripts
  match "scripts/**" $ do
    route   $ setExtension "js"
    -- compiles all typescript and javascript to the js target
    -- then compresses the result
    compile compressJtsCompiler
```

See the documentation for other available compilers.

## Build the Source

This project uses `stack`. Clone the project, then run `stack build` from the root.

## Build the Docs

For whatever reason `stack haddock` fails to pull down some necessary packages.
To build the docs run `stack exec haddock --  --html --hoogle --html-location=../$pkg-$version/ --hyperlinked-source --quickjump src/**/*.hs  -o doc`. The index
will be found in `./doc/`.
