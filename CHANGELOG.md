# Changelog

Notable changes to this project are documented in this file. The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/) and this project adheres to [Semantic Versioning](https://semver.org/spec/v2.0.0.html).

## [Unreleased]

Breaking changes:

New features:

Bugfixes:

Other improvements:

## [v8.1.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v8.1.1) - 2022-10-24

Other improvements:
- Use `EffectFn` throughout instead of unsafe `mkEffect` utility (#70 by @colinwahl)

## [v8.1.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v8.1.0) - 2022-06-10

New features:
- Add `lstat` (#66 by @artemisSystem)
- Add rmdir', which takes an take options arg (#67 by @wclr)
- Added rm and rm' version with and without options arg (#67 by @wclr)

## [v8.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v8.0.0) - 2022-04-29

Breaking changes:
- Remove `Async.exists` (#61 by @sigma-andex)
- Update `mkdir` to take an options record arg, exposing `recursive` option (#53, #55, #58 by @JordanMartinez)
  To get back the old behavior of `mkdir'`, you would call `mkdir' { recursive: false, mode: mkPerms all all all }`

New features:
- Update project and deps to PureScript v0.15.0 (#59 by @JordanMartinez, @thomashoneyman, @sigma-andex)
- Remove duplicate `node-buffer` from bower.json (@thomashoneyman)

## [v7.0.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v7.0.1) - 2022-04-27

Due to an incorrectly-implemented breaking change, use v8.0.0

## [v7.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v7.0.0) - 2022-04-27

Due to an incorrectly-implemented breaking change, use v8.0.0

## [v6.2.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v6.2.0) - 2022-01-10

Breaking changes:

New features:
- Add bindings to `mkdir(path, { recursive: true })` via `mkdirRecursive` (#53, #55 by @JordanMartinez)

Bugfixes:

Other improvements:

## [v6.1.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v6.1.0) - 2021-05-06

New features:
- Exported `mkPerm` (#42 by @JordanMartinez)

Other improvements:
- Fixed warnings revealed by v0.14.1 PS release (#42 by @JordanMartinez)

## [v6.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v6.0.0) - 2021-02-26

Breaking changes:
- Added support for PureScript 0.14 and dropped support for all previous versions (#46)
- Dropped deprecated `globals` dependency (#47)

Other improvements:
- Migrated CI to GitHub Actions, updated installation instructions to use Spago, and migrated from `jshint` to `eslint` (#45)
- Added a changelog and pull request template (#49)

## [v5.0.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v5.0.1) - 2019-07-24

- Relaxed upper bound on `node-buffer`

## [v5.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v5.0.0) - 2018-05-29

Updates for 0.12

## [v4.0.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v4.0.1) - 2018-03-05

- Raised upper bounds on `purescript-js-date` dependency (@justinwoo)

## [v4.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v4.0.0) - 2017-04-04

Updates for 0.11 (@anilanar)

## [v3.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v3.0.0) - 2016-10-21

- Updated dependencies

## [v2.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v2.0.0) - 2016-07-31

- Updated dependencies

## [v1.0.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v1.0.0) - 2016-06-11

Update for 1.0 core libraries and PureScript 0.9.

## [v0.11.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.11.0) - 2016-03-31

Bump dependencies (`purescript-node-streams` → ~0.4.0).

## [v0.10.2](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.10.2) - 2016-03-31

Update README

## [v0.10.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.10.1) - 2016-03-05

- Fix exceptions thrown by `Node.FS.Sync.stat` being uncatchable

## [v0.10.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.10.0) - 2015-12-18

- Add functions for accessing the filesystem using Node.js streams (#13, #26)

## [v0.9.2](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.9.2) - 2015-12-02

- Fixed warnings (@thimoteus)

## [v0.9.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.9.1) - 2015-11-13

- Add Show and Eq instances for FileFlags
- Improved documentation

## [v0.9.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.9.0) - 2015-11-12

- Added functions to `Node.FS.Async` for dealing with file descriptors asynchronously. (@timbod7, #21)
- Change the `Show` instances for `SymlinkType` to obey the informal `Show` law of getting executable PureScript code out.
- Added a new function `symlinkTypeToNode` for representing a `SymlinkType` as a `String` for use with Node.js APIs (this was previously the `Show` instance).
- ~~Added a `Show` instance for `FileFlags`.~~ (this is in v0.9.1 because @hdgarrood forgot to do this)
- Added a `fileFlagsToNode` function for representing a `FileFlags` value as a `String` for use with Node.js APIs.
- The `FileDescriptor` and `FileFlags` types, and all relevant type synonyms, have been moved from `Node.FS.Sync` to `Node.FS`.
- Update to version ^0.2.0 of `purescript-node-buffer`
- Use `Int` instead of `Number` where appropriate in the following functions:
  - `chown`
  - `truncate`
  - `utimes`
- All warnings have been fixed. (@timbod7)

## [v0.8.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.8.1) - 2015-08-14

- Updated dependencies

## [v0.8.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.8.0) - 2015-08-14

- Updated dependencies

## [v0.7.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.7.1) - 2015-07-06

Fix type of `Async.exists`.

## [v0.6.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.6.0) - 2015-05-24

- Use a proper type for permissions (@felixSchl)

## [v0.5.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.5.0) - 2015-04-07

- Update dependencies

## [v0.4.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.4.0) - 2015-02-21

**This release requires PureScript v0.6.8 or later**
- Updated dependencies

## [v0.3.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.3.0) - 2015-01-29

- Updated to use latest `purescript-datetime`

## [v0.2.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.2.1) - 2015-01-14

- Added file descriptor features (@dysinger)

## [v0.2.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.2.0) - 2015-01-10

- Update `purescript-foreign` dependency (@garyb)

## [v0.1.3](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.1.3) - 2014-11-18

- Fix duplicate labels rows (@paf31)

## [v0.1.2](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.1.2) - 2014-10-14

- Fixed dependency versions (@jdegoes)

## [v0.1.1](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.1.1) - 2014-09-10

- Add `exists` (@joneshf)
- Actions in `Async` no longer evaluate too early (@garyb / @paf31)

## [v0.1.0](https://github.com/purescript-node/purescript-node-fs/releases/tag/v0.1.0) - 2014-08-11

Initial semver release.
