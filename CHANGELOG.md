# Changelog

All notable changes to this project will be documented in this file.

## [Unreleased]

### Fixed

- Prevented call environments from being retained by directly bound local closures and local
  functions.
- Shared lambda payloads across value clones to avoid repeatedly cloning function ASTs.
