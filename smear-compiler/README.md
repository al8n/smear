# `smear-compiler`

[![Crates.io](https://img.shields.io/crates/v/smear-compiler.svg)](https://crates.io/crates/smear-compiler)
[![Documentation](https://docs.rs/smear-compiler/badge.svg)](https://docs.rs/smear-compiler)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

What a parsed document is compiled into: the built-once GraphQL schema, with the draft §3 builder
and the draft §5 validator that read it, and the GraphQLx schema analyzer.

## The layers

- **`schema`** is the runtime layer, and it is always compiled: the built-once `Schema`, with
  interned names, flat tables, packed type references and possible-object bitsets, the diagnostic
  vocabulary and the specified definitions. It validates nothing about a *document* and parses
  nothing.
- **`graphql`**, behind the feature of that name, is GraphQL's compiler: draft §3 "Type
  Validation" inside `Schema::build`, the draft §4 introspection door behind `introspection`, and
  draft §5 document validation. Standard GraphQL only: the GraphQLx dialect's generics, `where`
  constraints, set and map literals and namespaced paths have no specification semantics to
  validate against, so nothing in it knows about them.
- **`graphqlx`**, behind the feature of that name and a tier, is GraphQLx's: packages in, a
  resolved model and its diagnostics out.
- **A private kernel** holds what the builder and the analyzer share: a byte-spelling interner,
  duplicate-name detection and a graph-cycle kernel. Nothing outside the crate names it.

The validator works on three axes:

- **The schema** is built once and read as `&Schema` by an unbounded number of concurrent
  validations.
- **The report** is one seam: `Sink::diagnostic` takes a value and returns whether to keep going.
- **The working set** is the caller's `Scratch`, reused across requests, and what may be *done* is
  the caller's `Budget`. The steady state allocates nothing at all, which
  `smear/tests/validator_allocation.rs` measures with a counting allocator rather than asserting.

The validator reads the document either of `smear-parser`'s parser layers answers, or one the
caller builds. The lossless layer, which recovers and keeps the tree, is `smear-parser`'s, behind
its `rowan_0_17` and `std` features; this crate declares no feature for it.

## Feature flags

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The allocating API without the standard library: the schema representation, and the heap code of each compiler that is on. `std` gives the same API; with neither, the crate is `no_std` and keeps only `schema::diagnostic` and `schema::builtin`. Implied by no feature | |
| `std` | Standard library support, and with it the allocating API; it does not turn `alloc` on. Off, the crate is `no_std` | ✓ |
| `graphql` | GraphQL's compiler: `Schema::build`, `SchemaBuilder` and `SchemaError`, the SDL door, in `schema`, and the draft §5 rules in the `graphql` module. Pulls `smear-parser`. Needs `std` or `alloc` | |
| `introspection` | `Schema::from_introspection`, the draft §4 door, and the `schema::introspection` module. Implies `graphql`; needs `std` or `alloc` | |
| `graphqlx` | The GraphQLx schema analyzer, the `graphqlx` module. Pulls `smear-lexer` and `smear-parser`. Needs `std` or `alloc` | |
| `bytes_1` | The analyzer reads `bytes::Bytes` sources, from `bytes` 1. Implies no tier and no compiler | |
| `bstr_1` | The analyzer reads `bstr::BStr` sources, from `bstr` 1. Implies no tier and no compiler | |
| `hipstr_0_8` | The analyzer reads the `hipstr::{HipStr, HipByt}` sources, from `hipstr` 0.8. Implies no tier and no compiler | |
| `smol-bytes_0_2` | The analyzer reads the smol-bytes 0.2 sources. Implies no tier and no compiler | |
| `test-support` | `graphqlx::test_support`: a hidden door to the crate-private model builder, and to analyses of the models it builds, for `smear`'s own tests. Not API; `smear`'s `test-support` forwards it | |

No feature but `default` switches a tier on, so a `default-features = false` consumer names `std`
or `alloc` itself. Each source feature forwards `smear-parser`'s of the same name weakly: it pulls
no parser of its own, and without `graphqlx` it compiles nothing more. `bytes`, `bstr`, `hipstr` and
`smol-bytes` are aliases for the current version: each enables its versioned feature and nothing
else.

What each combination compiles:

| Features | Compiled | Dependencies beyond `tokora` and `derive_more` |
|---|---|---|
| none | `schema::diagnostic` and `schema::builtin` | none |
| a tier | adds the schema representation and its owned text | smol-bytes, where the target has pointer-width atomics |
| a tier and `graphql` | adds the builder and the validator | `smear-parser`, and `smear-lexer` under it |
| a tier and `introspection` | adds `schema::introspection` as well | the same |
| a tier and `graphqlx` | adds the analyzer | `smear-lexer` and `smear-parser` |

## Reading a schema without a front end

Three consumers, and they do not want the same thing:

- **The validator works on a schema the builder builds**, so `graphql` puts `smear-parser` under
  both.
- **`graphql-proto` reads one** at execution time, and **`smear-derive` will generate from one**.
  Neither needs a front end, so both depend on this crate with `default-features = false` and a
  tier, `std` or `alloc`, and get a `core` + `alloc` representation with `tokora` as its
  dependency, plus smol-bytes on a target with pointer-width atomics, for the owned text the crate
  shares with `graphql-proto`.

The representation needs nothing but `core` and `alloc`, and that is a build rather than a claim:
the `smear-noatomic` workspace member compiles this crate's own `schema/repr/` files for
`thumbv6m-none-eabi`, a core with no compare-and-swap.

## Why the builder is a feature and not another crate

`Schema::build` and `Schema::from_introspection` are **inherent** `impl Schema` blocks, and an
inherent impl cannot cross a crate boundary. Splitting the representation out without them is not a
move at all: it costs an extension trait per body and a `use` line at every call site. A feature
buys the same tier with no API change.

## The analyzer

GraphQLx only. Standard GraphQL is validated against the draft specification by the `graphql`
module; the analyzer resolves what GraphQLx adds, its modules, `use` items, generics and packages,
and checks a GraphQLx program against smear's own rules and the rules GraphQLx shares with GraphQL.

### What goes in, and what comes out

- **Packages:** a root package and its dependencies, each a name, a layout (`Directory` or `Flat`),
  a tier and its files' texts (`&str`, `&[u8]` or a source feature's type). The caller reads the
  files and supplies every package: the analysis does no I/O.
- **Limits:** `graphqlx::Limits`, the bounds untrusted input runs under.
- **Options:** `graphqlx::Options`, the switches that relax or tighten what the language accepts,
  each off by default.
- **The model:** dense tables read through `Copy` views, of packages, modules, definitions,
  members, type occurrences and interned types, values, directive applications, the schema and the
  built-ins, with canonical names and lookup. It owns every spelling it answers with.
- **The diagnostics,** in one total order under a ceiling, each with a stable code, and the source
  map that pairs them with the caller's texts again for a renderer.
- **The verdict,** `Analysis::is_ok`: whether any error was reported, whether the ceiling kept it
  or not, or counted by a parser that did not lend it.

### What it checks

| Step | What it does | Codes |
|---|---|---|
| The package set | Numbers the packages and their sources, and checks their names, file paths, dependencies and size | `smear::graphqlx`: 7 |
| Parsing | Reads every file that holds a token with the parser layer chosen | `smear::syntax`: the parser's; `smear::graphqlx`: 1 |
| Collection | Builds each package's module tree and collects what each module declares | `smear::graphqlx`: 4 |
| Declaration | Numbers every module and definition, and checks each module's names | `smear::schema`: 5; `smear::graphqlx`: 1 |
| Name resolution | Builds every module's scope, resolves its `use` items, and resolves each path where it stands | `smear::schema`: 6; `smear::graphqlx`: 11 |
| Lowering | Writes what each definition holds: its contributions, type parameters, members, type occurrences and applications, its values as written, and the schema | `smear::schema`: 4; `smear::graphqlx`: 5 |
| Instantiation | Gives every type occurrence its type and every application its complete type arguments, and judges the bounds of every instantiation the program writes | `smear::graphqlx`: 4 |
| Coercion | Coerces every value to the type expected where it stands, filling what is left out from its default; a value of a custom scalar that is no collection but `null`, or of a type that still names a type parameter, is kept as written, and so is an extern collection's other literal under `Options::extern_collections_as_written` | `smear::schema`: 6; `smear::graphqlx`: 4 |
| Binding | Binds each scalar to its host types | none |
| Implementation | Checks what each definition says it implements, transitively and field by field, and a union's members | `smear::schema`: 9; `smear::graphqlx`: 1 |
| Tiers | Marks what only compile-time metadata reaches, and gives every definition and every type the tier it requires | none |
| smear's rules | 33 rules, each a visitor over the model: smear's own on GraphQLx, and the type-system rules GraphQLx shares with GraphQL | `smear::graphqlx`: 11; `smear::schema`: 39 |

`graphqlx::DiagnosticKind::ALL` lists the 39 codes of the analyzer's own family, and
`graphqlx::schema::ALL` the 68 of GraphQL's it shares; the module's documentation names each
phase's codes and each rule's.

On every GraphQL document of smear's corpora, given as the one file of a `Flat` package, the
analyzer gives the schema builder's verdict, but for three capacity ceilings of GraphQL's builder,
which GraphQLx has only as `Limits` a consumer tightens to them. A differential test holds it
there, code by code, lists each count it lets differ and why, and holds that under those limits the
analyzer refuses each document the builder refuses for a ceiling. That is this release's rules:
GraphQLx is not bound to GraphQL's.

### Rules of your own

A rule is a [`graphqlx::Visitor`](https://docs.rs/smear-compiler/latest/smear_compiler/graphqlx/trait.Visitor.html):
it implements the hooks it needs, `graphqlx::visit` walks the model for it over the packages it
selects, and it reports through the walk's context with codes of its own, which render as smear's
do. Rules compose at compile time, and a consumer's verdict is its own conjunction with
`Analysis::is_ok`. One rule ships for a backend to compose, and `analyze` never runs it:
`graphqlx::UnboundedInstantiation` refuses a generic definition that reaches itself with a type
argument that grows, for a backend that makes one copy per instantiation.

### Limits

What the analysis builds and spends once it has numbered the packages it is given is bounded by
[`graphqlx::Limits`](https://docs.rs/smear-compiler/latest/smear_compiler/graphqlx/struct.Limits.html):
a bound passed stops that work, is reported as `smear::graphqlx::resource-limit`, and the analysis
goes on, but for the diagnostic ceiling and the near-miss budget, which limit what the analysis says
and not what it accepts. A third kind refuses a shape and stops no work: how many arguments a field
and a directive declare, and how many list and non-null wrappers a written type has. They cost the
analysis no more than their text, and a consumer sets them for a representation of its own: at 64,
64 and 15 they refuse each document GraphQL's schema builder refuses for a ceiling. Numbering and
ordering the packages comes first, under no bound but the
diagnostic ceiling: each package's files are sorted by path, two files that share a path are
ordered by their texts, two packages that share a name by everything they hold, and every name and
path is copied and checked. That work grows with everything the caller supplies, texts included,
and only the caller can bound it.

### Stability

The `graphqlx` module makes no semver promise yet: smear has no downstream user, so a release may
change any of its paths, signatures or answers, and the CHANGELOG says each change.

## Part of the Smear Ecosystem

- `smear-lexer` — lexical analysis
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-compiler` — the built-once schema, its §3 builder, §5 validation and the GraphQLx analyzer (this crate)
- `graphql-proto` — the Sans-I/O protocol machine
- `smear` — the umbrella that re-exports all of them, each behind a feature

A consumer of the umbrella reaches the validator as `smear::validator` and the schema as
`smear::validator::schema`, behind `validator`; the analyzer as `smear::analysis::graphqlx`, behind
`analysis`; and the diagnostic vocabulary as `smear::diagnostic`.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
