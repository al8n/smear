# `smear-parser`

[![Crates.io](https://img.shields.io/crates/v/smear-parser.svg)](https://crates.io/crates/smear-parser)
[![Documentation](https://docs.rs/smear-parser/badge.svg)](https://docs.rs/smear-parser)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

Atomic parser-combinators for GraphQL-family dialects.

Builds AST nodes from the token streams `smear-lexer` produces, composed with the `tokora`
combinator library, plus the lossless CST tower behind the `rowan_0_17` feature.

## Feature flags

Every crate in the stack has three tiers, each including the one below: core, `alloc` and `std`.

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The parsers and ASTs of both dialects. Off, the crate is `no_std` and keeps only `combinator` (with `typed_keyword_atom!`) and the `lexer` re-export. Implied by `std`, `smallvec_1`, `bytes_1`, `hipstr_0_8` and `smol-bytes_0_2` | via `std` |
| `std` | Standard library support. Implies `alloc`; off, the crate is `no_std` | ✓ |
| `rowan_0_17` | The lossless CST tower, over `rowan` 0.17. Implies `std` | |

The dialects (`graphql`, `graphqlx`) and the other features are the ones `smear`'s own table
describes. A capability feature never switches a tier on, so a `default-features = false` consumer
names `alloc` itself, or `std`, which implies it.

The dependency features (`smallvec_1`, `bytes_1`, `bstr_1`, `hipstr_0_8`, `smol-bytes_0_2` and
`rowan_0_17`) carry the major of the crate they enable. The unversioned names — `smallvec`, `bytes`,
`bstr`, `hipstr`, `smol-bytes` and `rowan` — are aliases for the current version: each enables its
versioned feature and nothing else.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower (this crate)
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear` — the umbrella that re-exports all of them, each behind a feature

A consumer of the umbrella reaches this crate as `smear::parser`, which is the path it had while
the two were one crate (#84), so nothing written against that spelling has to move.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
