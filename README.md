# Haskell Accumulator

Haskell bindings for the Rust accumulator library via C. This library provides cryptographic accumulator functionality with support for polynomial commitments over BLS12-381 curve groups (G1 and G2).

## Overview

This library provides:
- **Accumulator operations**: Add, remove, and query elements in a cryptographic accumulator
- **Polynomial commitments**: Generate proofs of membership using polynomial commitments over G1 and G2
- **BLS12-381 support**: Integration with Cardano's BLS12-381 elliptic curve implementation

## Prerequisites

- **Nix** (recommended): For reproducible builds and dependency management
- **Cabal**: Version 3.0 or later
- **GHC**: Compatible with GHC 9.6.x
- **pkg-config**: Required for linking with the Rust accumulator library

## Building the Project

The project uses Nix flakes for reproducible builds. This is the recommended approach as it handles all dependencies, including the Rust accumulator library.

### Enter the development shell:

```bash
nix develop
```

This will provide you with:
- `cabal-install`
- `haskell-language-server`
- `ghcid`
- All necessary build dependencies

### Build the project:

```bash
cabal build
```

## Running Benchmarks

The project includes comprehensive benchmarks that measure performance of polynomial commitment operations over G1 and G2 groups.

From within the Nix development shell:

```bash
# Run the test suite (includes benchmarks and E2E example)
cabal test bindings-test

# Or run directly (includes benchmarks and E2E example)
cabal run bindings-test
```

The Criterion benchmark suite supports various command-line options. Pass them after `--`:

```bash
# Generate HTML report
cabal run bindings-test -- --output benchmark.html
```
