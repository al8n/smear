# `smear-schema`

[![Crates.io](https://img.shields.io/crates/v/smear-schema.svg)](https://crates.io/crates/smear-schema)
[![Documentation](https://docs.rs/smear-schema/badge.svg)](https://docs.rs/smear-schema)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The built-once GraphQL schema: interned names, flat tables, packed type references and
possible-object bitsets, plus the draft §3 "Type Validation" builder that produces one.

Three consumers, and they do not want the same thing:

- **`smear-compiler` builds a schema**, so it enables `build` and gets `smear-parser` underneath.
- **`graphql-proto` reads one** at execution time, and **`smear-derive` will generate from one** —
  neither needs a front end, so both depend on this crate with `default-features = false` and get
  `core` + `alloc` with `tokora` as the only dependency.

It validates nothing about a *document* and parses nothing. Draft §5 validation is
`smear-compiler`'s.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support (off = `no_std`; `alloc` is required either way) | ✓ |
| `build` | `Schema::build`, `SchemaBuilder` and `SchemaError` — the SDL door. Pulls `smear-parser` | |
| `introspection` | `Schema::from_introspection` — the draft §4 door. Implies `build`, adds `serde` | |

## Why the builder is a feature and not another crate

`Schema::build` and `Schema::from_introspection` are **inherent** `impl Schema` blocks, and an
inherent impl cannot cross a crate boundary. Splitting the representation out without them is not
a move at all — it costs an extension trait per body and a `use` line at every call site. A
feature buys the same tier with no API change.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-schema` — the built-once schema representation and its builder (this crate)
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear` — the umbrella that re-exports all of them, each behind a feature

A consumer of the umbrella reaches this crate as `smear::validator::schema`, and its diagnostic
vocabulary as `smear::diagnostic`.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
