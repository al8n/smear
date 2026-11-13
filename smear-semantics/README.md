# `smear-semantics`

[![Crates.io](https://img.shields.io/crates/v/smear-semantics.svg)](https://crates.io/crates/smear-semantics)
[![Documentation](https://docs.rs/smear-semantics/badge.svg)](https://docs.rs/smear-semantics)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The algorithms smear's two semantic layers share: the GraphQL schema builder in `smear-schema` and
the GraphQLx analyzer in `smear-analysis`.

- `interner` — byte spellings interned into dense symbols, refused rather than wrapped at the
  arena's two ceilings.
- `duplicates` — the earlier position of a name list that wrote a given name, scanned while the
  list is narrow and sorted once when it is wide.
- `scc` — which nodes of a directed graph lie on a cycle, and which strongly connected component
  each node is in, each found in one walk over compressed rows.

Each is generic over its caller's symbol type and names nothing of either layer.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The three kernels, without the standard library. `std` gives the same API; with neither, the crate is `no_std` and empty. Implied by no feature | |
| `std` | Standard library support, and with it the three kernels; it does not turn `alloc` on. Off, the crate is `no_std` | ✓ |

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-semantics` — the algorithms the semantic layers share (this crate)
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear-analysis` — the GraphQLx schema analyzer
- `smear` — the umbrella that re-exports the layers, each behind a feature

`smear` does not re-export this crate: it is reached through the layers built on it.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
