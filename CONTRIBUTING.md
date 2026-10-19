# Contributing to Graft

## Build and test

```sh
cabal build all --ghc-options=-Werror
cabal test all --test-show-details=direct
cabal haddock lib:graft
```

Graft is tested with GHC 9.8, 9.10, 9.12 and 9.14 (see `tested-with` in
`graft.cabal`). There are three test suites:

- `graft-tests` checks the library against independent models.
- `graft-example` builds and runs [`examples/ExactUpdate.hs`](examples/ExactUpdate.hs),
  so changes to the API that break the example break the build.
- `graft-doctest` checks every `>>>` example in the Haddock documentation. It
  is off by default because it needs the `doctest` package and a GHC
  environment file, which cabal writes at the end of a build:

  ```sh
  cabal build all -fdoctest
  cabal test graft-doctest -fdoctest
  ```

  On a fresh checkout the first `cabal test` would run before the file exists,
  so build first; the suite says so if the file is missing. The repository's
  `cabal.project` makes cabal write the file; from a source tarball, add
  `--write-ghc-environment-files=always` to the build. Examples in
  `@`-blocks and `>`-blocks are not checked, so keep those short and compile
  them by hand when they change.

## How the tests work

Each property compares the library with a model that shares none of its code.
`test/TestModel.hs` defines a plain rose tree of `String`s with no cached
lengths and its own UTF-8 width function. Generated text includes multibyte
characters, combining marks, CRLF, NUL and empty tokens, and shrinking never
produces surrogate code points. Every property runs 1,000 cases, and
`checkCoverage` confirms that the generators reach the interesting cases (root
and nested targets, nodes and tokens).

The suites are grouped by module:

- `test/Main.hs`: rendering, cached lengths, path edits, cursor navigation,
  offset lookup, covering ranges and edits through cursors.
- `test/BuilderTests.hs`: builders, checked against a separate tree model that
  streams start, token and finish events, including checkpoint wrapping and the
  `Build` monad.
- `test/ChildEditTests.hs`: splicing, insertion and removal, including shifted
  paths and ranges and the order in which errors are reported.
- `test/AstTests.hs`: typed-view laws, fields, ancestors and edits through views.
- `test/LineIndexTests.hs`: line and column conversion against a `String` oracle.

Each group ends with deterministic regressions for edge cases: empty nodes and
tokens, extreme indices such as `maxBound`, and shared trees whose logical length
(`2 ^ hugeDepth` bytes) is far larger than anything that could be rendered.

## Benchmarks

[`bench/README.md`](bench/README.md) explains how to run the benchmarks and
compare results. [`docs/performance.md`](docs/performance.md) records the
current baseline. Rerun the baseline when a change affects performance, and keep
the raw results under `bench/results/<date>/` rather than editing tables by hand.

## Commits

Keep each commit to one concern, and write messages in
[Conventional Commits](https://www.conventionalcommits.org/) form.
