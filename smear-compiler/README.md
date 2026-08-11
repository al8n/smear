# `smear-compiler`

[![Crates.io](https://img.shields.io/crates/v/smear-compiler.svg)](https://crates.io/crates/smear-compiler)
[![Documentation](https://docs.rs/smear-compiler/badge.svg)](https://docs.rs/smear-compiler)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

Draft §5 GraphQL document validation, over the built-once schema `smear-schema` produces.

Standard GraphQL only. The `graphqlx` dialect's generics, `where` constraints, map and set types
and namespaced paths have no specification semantics to validate against, so nothing here knows
about them.

Three axes:

- **The schema** is built once and read as `&Schema` by an unbounded number of concurrent
  validations.
- **The report** is one seam: `Sink::diagnostic` takes a value and returns whether to keep going.
- **The working set** is the caller's `Scratch`, reused across requests, and what may be *done* is
  the caller's `Budget`. The steady state allocates nothing at all, which
  `smear/tests/validator_allocation.rs` measures with a counting allocator rather than asserting.

## Not named `smear-validator`

It would misdescribe the contents: the crate also carries the rule vocabulary, the diagnostic
rendering and the caller of the §3 build. A name that misdescribes its contents is a defect.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support (off = `no_std`; `alloc` is required either way) | ✓ |
| `rowan` | The lossless door — the same rules over a rowan CST, by projection | |
| `introspection` | Re-exports `smear-schema`'s draft §4 construction door and its error type | |

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics (this crate)
- `graphql-proto` — the Sans-I/O protocol machine
- `smear` — the umbrella that re-exports all of them, each behind a feature

A consumer of the umbrella reaches this crate as `smear::validator`.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
