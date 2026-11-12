# `smear-analysis`

[![Crates.io](https://img.shields.io/crates/v/smear-analysis.svg)](https://crates.io/crates/smear-analysis)
[![Documentation](https://docs.rs/smear-analysis/badge.svg)](https://docs.rs/smear-analysis)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The GraphQLx schema analyzer: GraphQLx packages in, a resolved model and its diagnostics out.

GraphQLx only. Standard GraphQL is validated against the draft specification by `smear-compiler`;
the dialect's modules, imports, generics and packages are what this crate resolves.

`graphqlx::analyze` takes a root package and its dependencies, numbers their sources, checks the
package set, parses every file and runs smear's own rules over the supplied packages. It answers an
`Analysis`: the model, the diagnostics in one total order, and the source map that pairs the
analysis with the caller's texts again. The model is dense tables read through `Copy` views over
packages, modules, definitions, members, type occurrences and types, values, directive
applications, the schema and the built-ins, with canonical names and lookup; so far `analyze` fills
in only the packages and their root modules.

Rules are visitors. A `graphqlx::Visitor` implements the hooks it needs, one for each entity a walk
enters, leaves or reaches, and `graphqlx::visit` walks an analysis's model for it over the packages
`Packages` selects, handing each hook a `Cx` that says where the walk is and takes what the visitor
reports. Visitors compose at compile time: a tuple of up to twelve, an `Option` of one, a `&mut` or
a `Box` of one, and `()`. `analyze` runs smear's own rule set, which holds no rule yet, through the
same walk, and an error that a phase or a rule reports marks the contributions, members and
definitions that hold it, as their `has_errors()` says, whether the diagnostic ceiling keeps it or
not. A consumer's walk marks nothing, answers its own diagnostics under the same ceiling, and holds
memory bounded by it.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The analyzer, the `graphqlx` module, without the standard library. `std` gives the same API; with neither, the crate is `no_std` and exports nothing. Implied by no feature | |
| `std` | Standard library support, and with it the analyzer; it does not turn `alloc` on. Off, the crate is `no_std` | ✓ |
| `bytes_1` | Read `bytes::Bytes` sources, from `bytes` 1. Implies no tier | |
| `bstr_1` | Read `bstr::BStr` sources, from `bstr` 1. Implies no tier | |
| `hipstr_0_8` | Read the `hipstr::{HipStr, HipByt}` sources, from `hipstr` 0.8. Implies no tier | |
| `smol-bytes_0_2` | Read the smol-bytes 0.2 sources. Implies no tier | |
| `test-support` | `graphqlx::test_support`: a hidden door to the crate-private model builder, and to analyses of the models it builds, for `smear`'s own tests. Not API; `smear`'s `test-support` forwards it | |

Each source feature forwards `smear-parser`'s of the same name and implies no tier, so a
`default-features = false` consumer names `std` or `alloc` beside it. `bytes`, `bstr`, `hipstr` and
`smol-bytes` are aliases for the current version: each enables its versioned feature and nothing
else.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-semantics` — the algorithms the semantic layers share
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear-analysis` — the GraphQLx schema analyzer (this crate)
- `smear` — the umbrella that re-exports the layers, each behind a feature

A consumer of the umbrella reaches this crate as `smear::analysis`, behind its `analysis` feature.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
