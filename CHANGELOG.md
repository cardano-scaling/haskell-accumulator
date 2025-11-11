# Changelog

The format is based on [Keep a Changelog](https://keepachangelog.com/en/1.0.0/),
and this project adheres to [PVP Versioning](https://pvp.haskell.org).

## [1.1.0.0] - 2025-11-11

### Changed

- Removed `IO` from API functions `getPolyCommitOverG1` and
  `getPolyCommitOverG2` in the `Bindings` module. These functions now return
  `Either String Point1` and `Either String Point2` directly instead of `IO`.

  See [PR #7](https://github.com/cardano-scaling/haskell-accumulator/pull/7) for details.

## [1.0.0.1]

- Remove custom dependencies, instead use recent CHaP releases.

## [1.0.0.0]

### Added
- Initial release of haskell-accumulator
- Core accumulator data structure and operations
- Polynomial commitment bindings for G1 and G2 groups
- BLS12-381 curve integration
- Benchmark suite using Criterion
