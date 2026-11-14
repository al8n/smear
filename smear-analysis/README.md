# `smear-analysis`

[![Crates.io](https://img.shields.io/crates/v/smear-analysis.svg)](https://crates.io/crates/smear-analysis)
[![Documentation](https://docs.rs/smear-analysis/badge.svg)](https://docs.rs/smear-analysis)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

The GraphQLx schema analyzer: GraphQLx packages in, a resolved model and its diagnostics out.

GraphQLx only. Standard GraphQL is validated against the draft specification by `smear-compiler`;
this crate resolves what GraphQLx adds, its modules, `use` items, generics and packages, and checks
a GraphQLx program against smear's own rules and the rules GraphQLx shares with GraphQL.

## What goes in, and what comes out

- **Packages:** a root package and its dependencies, each a name, a layout (`Directory` or `Flat`),
  a tier and its files' texts (`&str`, `&[u8]` or a source feature's type). The caller reads the
  files and supplies every package: the analysis does no I/O.
- **Limits:** `graphqlx::Limits`, the bounds untrusted input runs under.
- **The model:** dense tables read through `Copy` views, of packages, modules, definitions,
  members, type occurrences and interned types, values, directive applications, the schema and the
  built-ins, with canonical names and lookup. It owns every spelling it answers with.
- **The diagnostics,** in one total order under a ceiling, each with a stable code, and the source
  map that pairs them with the caller's texts again for a renderer.
- **The verdict,** `Analysis::is_ok`: whether any error was reported, whether the ceiling kept it
  or not, or counted by a parser that did not lend it.

## What it checks

| Step | What it does | Codes |
|---|---|---|
| The package set | Numbers the packages and their sources, and checks their names, file paths, dependencies and size | `smear::graphqlx`: 7 |
| Parsing | Reads every file that holds a token with the parser layer chosen | `smear::syntax`: the parser's; `smear::graphqlx`: 1 |
| Collection | Builds each package's module tree and collects what each module declares | `smear::graphqlx`: 4 |
| Declaration | Numbers every module and definition, and checks each module's names | `smear::schema`: 5; `smear::graphqlx`: 1 |
| Name resolution | Builds every module's scope, resolves its `use` items, and resolves each path where it stands | `smear::schema`: 6; `smear::graphqlx`: 11 |
| Lowering | Writes what each definition holds: its contributions, type parameters, members, type occurrences and applications, its values as written, and the schema | `smear::schema`: 4; `smear::graphqlx`: 4 |
| Instantiation | Gives every type occurrence its type and every application its complete type arguments, and judges the bounds of every instantiation the program writes | `smear::graphqlx`: 4 |
| Coercion | Coerces every value to the type expected where it stands, filling what is left out from its default; a value of a custom scalar that is no collection but `null`, or of a type that still names a type parameter, is kept as written | `smear::schema`: 6; `smear::graphqlx`: 4 |
| Binding | Binds each scalar to its host types | none |
| Implementation | Checks what each definition says it implements, transitively and field by field, and a union's members | `smear::schema`: 9; `smear::graphqlx`: 1 |
| Tiers | Gives every definition and every type the tier it requires | none |
| smear's rules | 33 rules, each a visitor over the model: smear's own on GraphQLx, and the type-system rules GraphQLx shares with GraphQL | `smear::graphqlx`: 11; `smear::schema`: 39 |

`graphqlx::DiagnosticKind::ALL` lists the 39 codes of the analyzer's own family, and
`graphqlx::schema::ALL` the 68 of GraphQL's it shares; the crate's documentation names each phase's
codes and each rule's.

On every GraphQL document of smear's corpora, given as the one file of a `Flat` package, the
analyzer gives `smear-schema`'s verdict, but for three capacity ceilings of `smear-schema`'s
representation that GraphQLx does not have. A differential test holds it there, code by code, and
lists each count it lets differ and why. That is this release's rules: GraphQLx is not bound to
GraphQL's.

## Rules of your own

A rule is a [`graphqlx::Visitor`](https://docs.rs/smear-analysis/latest/smear_analysis/graphqlx/trait.Visitor.html):
it implements the hooks it needs, `graphqlx::visit` walks the model for it over the packages it
selects, and it reports through the walk's context with codes of its own, which render as smear's
do. Rules compose at compile time, and a consumer's verdict is its own conjunction with
`Analysis::is_ok`. One rule ships for a backend to compose, and `analyze` never runs it:
`graphqlx::UnboundedInstantiation` refuses a generic definition that reaches itself with a type
argument that grows, for a backend that makes one copy per instantiation.

## Limits

Every resource that untrusted input could grow is bounded by
[`graphqlx::Limits`](https://docs.rs/smear-analysis/latest/smear_analysis/graphqlx/struct.Limits.html),
and a bound passed is `smear::graphqlx::resource-limit`, never a panic, but for the diagnostic
ceiling and the near-miss budget, which limit what the analysis says and not what it accepts.

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

## Stability

`smear-analysis` makes no semver promise yet: smear has no downstream user, so a release may change
any of its paths, signatures or answers, and the CHANGELOG says each change.

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
