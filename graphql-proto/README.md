# `graphql-proto`

[![Crates.io](https://img.shields.io/crates/v/graphql-proto.svg)](https://crates.io/crates/graphql-proto)
[![Documentation](https://docs.rs/graphql-proto/badge.svg)](https://docs.rs/graphql-proto)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The GraphQL protocol machine, Sans-I/O.

Draft §6 execution as a state machine: it takes events as inputs and emits actions as outputs,
owning no I/O, no clock and no randomness source. It says which field it needs and waits to be
told the answer.

It defines no value type. The driver keeps values in its own representation and answers structural
questions about them through the `Values` trait — the same choice `smear-lexer` makes about its
source, for the same reason: an owned enum would force an allocation per leaf and would make a
wasm or FFI handle second-class.

It depends on `smear-schema` with **default features off**, so reading a schema at execution time
does not put the front end in the graph.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine (this crate)
- `smear` — the umbrella that re-exports all of them, each behind a feature

A consumer of the umbrella reaches this crate as `smear::proto`.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
