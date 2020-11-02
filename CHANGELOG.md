# Changelog

All notable changes to this project will be documented here.

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [Haskell Package Versioning Policy](https://pvp.haskell.org/).

## [0.2.0] - 2020-11-02
### Added

- Using [hakyll-process](https://hackage.haskell.org/package/hakyll-process) to call the ts compiler.

### Changed

- Widened `hakyll` version bounds.

## [0.1.0] - 2019-09-29
### Added

- Initial Hakyll compilers for typescript and javascript.
  - Supports passing arbitrary arguments to the typescript compiler
  - Provides various helper compilers that pass commonly used typescript arguments
  - Supports minification of javascript source as a compiler
  - Provides helper compilers to compile typescript source to minified javascript.

