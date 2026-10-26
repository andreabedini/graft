# Contributing to Graft

## Build and test

```sh
cabal build all --enable-tests --enable-benchmarks --ghc-options=-Werror
cabal test all --enable-tests --enable-benchmarks --ghc-options=-Werror --test-show-details=direct
cabal haddock lib:graft
```

The test command repeats the build flags so that cabal doesn't reconfigure and
rebuild without `-Werror`.

Graft is tested with GHC 9.8, 9.10, 9.12 and 9.14 (see `tested-with` in
`graft.cabal`). There are four test suites:

- `graft-tests` checks the library against independent models.
- `graft-getting-started` builds and runs
  [`examples/GettingStarted.hs`](examples/GettingStarted.hs), the program in
  the README. Keep the two identical; CI compares them.
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
produces surrogate code points. Every property runs 1,000 cases. The
replacement and navigation properties use `checkCoverage` to confirm that the
generators reach root and nested targets, nodes and tokens.

The suites are grouped by module:

- `test/Main.hs`: rendering, cached lengths, replacement at every path,
  cursor navigation, offset lookup and covering ranges. Expectations are
  stated by path, through helpers in `TestModel.hs` written with cursors.
- `test/CursorEditTests.hs`: random programs of moves, replacements and
  lookups, run on a cursor and on the model, so that edits are checked while
  the cursor keeps moving.
- `test/FoldTests.hs`: `Green.foldM`, `fold`, `map` and `traverse` against the
  model, and forcing and hashing.
- `test/BuilderTests.hs`: builders, checked against a separate tree model that
  streams start, token and finish events, including checkpoint wrapping and the
  `Build` monad.
- `test/ChildEditTests.hs`: splicing, insertion and removal, including shifted
  paths and ranges and the order in which errors are reported.
- `test/AstTests.hs`: typed-view laws, fields, ancestors and edits through views.
- `test/LineIndexTests.hs`: line and column conversion against a `String` oracle.

Most groups end with deterministic regressions for edge cases: empty nodes and
tokens, extreme indices such as `maxBound`, and shared trees whose logical length
(`2 ^ hugeDepth` bytes) is far larger than anything that could be rendered.

## Benchmarks

[`bench/README.md`](bench/README.md) explains how to run the benchmarks and
compare results. [`docs/performance.md`](docs/performance.md) records the
current baseline. Rerun the baseline when a change affects performance, and keep
the raw results under `bench/results/<date>/` rather than editing tables by hand.

## Commit messages

Keep each commit to one concern.

Commit messages follow [Conventional Commits](https://www.conventionalcommits.org/). For example:

```
fix(green): fail on length overflow instead of wrapping
feat(green): map kinds with Functor and hash trees with Hashable
refactor(cursor)!: exclude the focus from ancestors
docs(cursor): state the real cost of a run of cursor edits
```

The allowed types are `feat`, `fix`, `perf`, `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` and `revert`. A scope in parentheses is optional. `!` before the colon, or a `BREAKING CHANGE:` footer, marks a breaking change. The `commits` CI job checks every commit of a pull request with [git-cliff](https://git-cliff.org/), so the allowed types are the ones that the commit parsers in `cliff.toml` know. There is no local commit hook, so this CI job is the only check.

The commit types choose the next version:

| Commits since the last release | Next version |
|---|---|
| At least one breaking change, of any type | `B` + 1 |
| At least one `feat` | `C` + 1 |
| At least one `fix` or `perf` | `D` + 1 |
| Only `docs`, `ci`, `test`, `refactor`, `build`, `chore`, `style` | No release |

Pull requests are merged with rebase merge. A squash merge would leave only the PR title for the release task to read.

## Versions

Versions follow the [Haskell PVP](https://pvp.haskell.org/) and have four components, `A.B.C.D`, for example `0.1.0.0`. `A.B` is the major version, `C` the minor version and `D` the patch version.

- git-cliff calculates the next version from the commit types, as in the table above. This is SemVer applied to `B.C.D`. There is no special rule for versions below 1.0: a breaking change increments `B` even when it is `0`.
- Incrementing a field sets all fields to its right to `0`.
- `A` changes only by hand, when the maintainer decides that a release deserves a new number. Like a change of `B`, it makes a major release.
- Release tags are `v` followed by the version, for example `v0.1.0.0`. The first release is `0.1.0.0`.
- Every package in `cabal.project` has the version of the release.

The `version:` fields in the `.cabal` files are the only source of the version. Only the release task, and the maintainer in a release PR, change them and `CHANGELOG.md`.

## Making a release

The maintainer makes releases.

1. Make sure that `main` is green.
2. Run `mise run release-pr`. It reads the commits since the last release tag, sets the next version in every `.cabal` file, regenerates `CHANGELOG.md` with [git-cliff](https://git-cliff.org/), and opens a pull request titled `chore(release): VERSION`. If there are no releasable commits, it says so and does nothing.
3. Review the pull request. To increment `A`, change the version by hand in every `.cabal` file, and change the `CHANGELOG.md` header and the commit description to the same version. Edits to `CHANGELOG.md` are lost at the next release, which regenerates it; wording that must stay goes in `cliff.toml`.
4. When `ci-ok` passes, merge the pull request with rebase merge.
5. CI's `release` job sees the release commit on `main`. It publishes a GitHub release with the source distribution `graft-VERSION.tar.gz` and its Hackage documentation `graft-VERSION-docs.tar.gz`, their `SHA256SUMS`, and the notes from `CHANGELOG.md`. Publishing creates the tag. Then the `pages` job publishes the documentation of the release to <https://andreabedini.github.io/graft/>.
6. Verify the release:

   ```
   gh release verify vVERSION
   gh attestation verify ASSET --repo andreabedini/graft
   ```

To publish a release on Hackage too, upload its source distribution and its documentation, which were built by CI, from the GitHub release:

```
gh release download vVERSION --repo andreabedini/graft --pattern 'PACKAGE-VERSION*.tar.gz'
cabal upload --publish PACKAGE-VERSION.tar.gz
cabal upload --publish --documentation PACKAGE-VERSION-docs.tar.gz
```

Releases are immutable: a published version cannot change. If a release is wrong, fix it and make a new release. If the `release` job fails before it publishes, rerun the failed jobs; the job continues the draft release.

### Release artifacts without a release

Every CI run, on a pull request or started by hand, uploads everything that a release would publish as one artifact, `dist`: the same files and `SHA256SUMS`, without attestations. To get them for any branch without releasing:

```
gh workflow run ci.yml --ref BRANCH
gh run list --workflow ci.yml --branch BRANCH --event workflow_dispatch --limit 1
gh run download RUN_ID --name dist
```

The `release` job runs only on a push to `main`, so a manual run never releases. The `github-pages` artifact of a run is the documentation site, as a tar file.
