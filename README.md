<div align="center">
<h1>Smear</h1>
</div>
<div align="center">

A zero-copy lexer, parser and validator for standard GraphQL, built out of reusable combinators so the same machinery serves GraphQL-like DSLs.

[<img alt="github" src="https://img.shields.io/badge/github-al8n/smear-8da0cb?style=for-the-badge&logo=Github" height="22">][Github-url]
<img alt="LoC" src="https://img.shields.io/endpoint?url=https%3A%2F%2Fgist.githubusercontent.com%2Fal8n%2F327b2a8aef9003246e45c6e47fe63937%2Fraw%2Fsmear" height="22">
[<img alt="Build" src="https://img.shields.io/github/actions/workflow/status/al8n/smear/ci.yml?logo=Github-Actions&style=for-the-badge" height="22">][CI-url]
[<img alt="codecov" src="https://img.shields.io/codecov/c/gh/al8n/smear?style=for-the-badge&token=6R3QFWRWHL&logo=codecov" height="22">][codecov-url]

[<img alt="docs.rs" src="https://img.shields.io/badge/docs.rs-smear-66c2a5?style=for-the-badge&labelColor=555555&logo=data:image/svg+xml;base64,PHN2ZyByb2xlPSJpbWciIHhtbG5zPSJodHRwOi8vd3d3LnczLm9yZy8yMDAwL3N2ZyIgdmlld0JveD0iMCAwIDUxMiA1MTIiPjxwYXRoIGZpbGw9IiNmNWY1ZjUiIGQ9Ik00ODguNiAyNTAuMkwzOTIgMjE0VjEwNS41YzAtMTUtOS4zLTI4LjQtMjMuNC0zMy43bC0xMDAtMzcuNWMtOC4xLTMuMS0xNy4xLTMuMS0yNS4zIDBsLTEwMCAzNy41Yy0xNC4xIDUuMy0yMy40IDE4LjctMjMuNCAzMy43VjIxNGwtOTYuNiAzNi4yQzkuMyAyNTUuNSAwIDI2OC45IDAgMjgzLjlWMzk0YzAgMTMuNiA3LjcgMjYuMSAxOS45IDMyLjJsMTAwIDUwYzEwLjEgNS4xIDIyLjEgNS4xIDMyLjIgMGwxMDMuOS01MiAxMDMuOSA1MmMxMC4xIDUuMSAyMi4xIDUuMSAzMi4yIDBsMTAwLTUwYzEyLjItNi4xIDE5LjktMTguNiAxOS45LTMyLjJWMjgzLjljMC0xNS05LjMtMjguNC0yMy40LTMzLjd6TTM1OCAyMTQuOGwtODUgMzEuOXYtNjguMmw4NS0zN3Y3My4zek0xNTQgMTA0LjFsMTAyLTM4LjIgMTAyIDM4LjJ2LjZsLTEwMiA0MS40LTEwMi00MS40di0uNnptODQgMjkxLjFsLTg1IDQyLjV2LTc5LjFsODUtMzguOHY3NS40em0wLTExMmwtMTAyIDQxLjQtMTAyLTQxLjR2LS42bDEwMi0zOC4yIDEwMiAzOC4ydi42em0yNDAgMTEybC04NSA0Mi41di03OS4xbDg1LTM4Ljh2NzUuNHptMC0xMTJsLTEwMiA0MS40LTEwMi00MS40di0uNmwxMDItMzguMiAxMDIgMzguMnYuNnoiPjwvcGF0aD48L3N2Zz4K" height="20">][doc-url]
[<img alt="crates.io" src="https://img.shields.io/crates/v/smear?style=for-the-badge&logo=data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBlbmNvZGluZz0iaXNvLTg4NTktMSI/Pg0KPCEtLSBHZW5lcmF0b3I6IEFkb2JlIElsbHVzdHJhdG9yIDE5LjAuMCwgU1ZHIEV4cG9ydCBQbHVnLUluIC4gU1ZHIFZlcnNpb246IDYuMDAgQnVpbGQgMCkgIC0tPg0KPHN2ZyB2ZXJzaW9uPSIxLjEiIGlkPSJMYXllcl8xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIiB4PSIwcHgiIHk9IjBweCINCgkgdmlld0JveD0iMCAwIDUxMiA1MTIiIHhtbDpzcGFjZT0icHJlc2VydmUiPg0KPGc+DQoJPGc+DQoJCTxwYXRoIGQ9Ik0yNTYsMEwzMS41MjgsMTEyLjIzNnYyODcuNTI4TDI1Niw1MTJsMjI0LjQ3Mi0xMTIuMjM2VjExMi4yMzZMMjU2LDB6IE0yMzQuMjc3LDQ1Mi41NjRMNzQuOTc0LDM3Mi45MTNWMTYwLjgxDQoJCQlsMTU5LjMwMyw3OS42NTFWNDUyLjU2NHogTTEwMS44MjYsMTI1LjY2MkwyNTYsNDguNTc2bDE1NC4xNzQsNzcuMDg3TDI1NiwyMDIuNzQ5TDEwMS44MjYsMTI1LjY2MnogTTQzNy4wMjYsMzcyLjkxMw0KCQkJbC0xNTkuMzAzLDc5LjY1MVYyNDAuNDYxbDE1OS4zMDMtNzkuNjUxVjM3Mi45MTN6IiBmaWxsPSIjRkZGIi8+DQoJPC9nPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPGc+DQo8L2c+DQo8Zz4NCjwvZz4NCjxnPg0KPC9nPg0KPC9zdmc+DQo=" height="22">][crates-url]
[<img alt="crates.io" src="https://img.shields.io/crates/d/smear?color=critical&logo=data:image/svg+xml;base64,PD94bWwgdmVyc2lvbj0iMS4wIiBzdGFuZGFsb25lPSJubyI/PjwhRE9DVFlQRSBzdmcgUFVCTElDICItLy9XM0MvL0RURCBTVkcgMS4xLy9FTiIgImh0dHA6Ly93d3cudzMub3JnL0dyYXBoaWNzL1NWRy8xLjEvRFREL3N2ZzExLmR0ZCI+PHN2ZyB0PSIxNjQ1MTE3MzMyOTU5IiBjbGFzcz0iaWNvbiIgdmlld0JveD0iMCAwIDEwMjQgMTAyNCIgdmVyc2lvbj0iMS4xIiB4bWxucz0iaHR0cDovL3d3dy53My5vcmcvMjAwMC9zdmciIHAtaWQ9IjM0MjEiIGRhdGEtc3BtLWFuY2hvci1pZD0iYTMxM3guNzc4MTA2OS4wLmkzIiB3aWR0aD0iNDgiIGhlaWdodD0iNDgiIHhtbG5zOnhsaW5rPSJodHRwOi8vd3d3LnczLm9yZy8xOTk5L3hsaW5rIj48ZGVmcz48c3R5bGUgdHlwZT0idGV4dC9jc3MiPjwvc3R5bGU+PC9kZWZzPjxwYXRoIGQ9Ik00NjkuMzEyIDU3MC4yNHYtMjU2aDg1LjM3NnYyNTZoMTI4TDUxMiA3NTYuMjg4IDM0MS4zMTIgNTcwLjI0aDEyOHpNMTAyNCA2NDAuMTI4QzEwMjQgNzgyLjkxMiA5MTkuODcyIDg5NiA3ODcuNjQ4IDg5NmgtNTEyQzEyMy45MDQgODk2IDAgNzYxLjYgMCA1OTcuNTA0IDAgNDUxLjk2OCA5NC42NTYgMzMxLjUyIDIyNi40MzIgMzAyLjk3NiAyODQuMTYgMTk1LjQ1NiAzOTEuODA4IDEyOCA1MTIgMTI4YzE1Mi4zMiAwIDI4Mi4xMTIgMTA4LjQxNiAzMjMuMzkyIDI2MS4xMkM5NDEuODg4IDQxMy40NCAxMDI0IDUxOS4wNCAxMDI0IDY0MC4xOTJ6IG0tMjU5LjItMjA1LjMxMmMtMjQuNDQ4LTEyOS4wMjQtMTI4Ljg5Ni0yMjIuNzItMjUyLjgtMjIyLjcyLTk3LjI4IDAtMTgzLjA0IDU3LjM0NC0yMjQuNjQgMTQ3LjQ1NmwtOS4yOCAyMC4yMjQtMjAuOTI4IDIuOTQ0Yy0xMDMuMzYgMTQuNC0xNzguMzY4IDEwNC4zMi0xNzguMzY4IDIxNC43MiAwIDExNy45NTIgODguODMyIDIxNC40IDE5Ni45MjggMjE0LjRoNTEyYzg4LjMyIDAgMTU3LjUwNC03NS4xMzYgMTU3LjUwNC0xNzEuNzEyIDAtODguMDY0LTY1LjkyLTE2NC45MjgtMTQ0Ljk2LTE3MS43NzZsLTI5LjUwNC0yLjU2LTUuODg4LTMwLjk3NnoiIGZpbGw9IiNmZmZmZmYiIHAtaWQ9IjM0MjIiIGRhdGEtc3BtLWFuY2hvci1pZD0iYTMxM3guNzc4MTA2OS4wLmkwIiBjbGFzcz0iIj48L3BhdGg+PC9zdmc+&style=for-the-badge" height="22">][crates-url]
<img alt="license" src="https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg?style=for-the-badge&fontColor=white&logoColor=f5c076&logo=data:image/svg+xml;base64,PCFET0NUWVBFIHN2ZyBQVUJMSUMgIi0vL1czQy8vRFREIFNWRyAxLjEvL0VOIiAiaHR0cDovL3d3dy53My5vcmcvR3JhcGhpY3MvU1ZHLzEuMS9EVEQvc3ZnMTEuZHRkIj4KDTwhLS0gVXBsb2FkZWQgdG86IFNWRyBSZXBvLCB3d3cuc3ZncmVwby5jb20sIFRyYW5zZm9ybWVkIGJ5OiBTVkcgUmVwbyBNaXhlciBUb29scyAtLT4KPHN2ZyBmaWxsPSIjZmZmZmZmIiBoZWlnaHQ9IjgwMHB4IiB3aWR0aD0iODAwcHgiIHZlcnNpb249IjEuMSIgaWQ9IkNhcGFfMSIgeG1sbnM9Imh0dHA6Ly93d3cudzMub3JnLzIwMDAvc3ZnIiB4bWxuczp4bGluaz0iaHR0cDovL3d3dy53My5vcmcvMTk5OS94bGluayIgdmlld0JveD0iMCAwIDI3Ni43MTUgMjc2LjcxNSIgeG1sOnNwYWNlPSJwcmVzZXJ2ZSIgc3Ryb2tlPSIjZmZmZmZmIj4KDTxnIGlkPSJTVkdSZXBvX2JnQ2FycmllciIgc3Ryb2tlLXdpZHRoPSIwIi8+Cg08ZyBpZD0iU1ZHUmVwb190cmFjZXJDYXJyaWVyIiBzdHJva2UtbGluZWNhcD0icm91bmQiIHN0cm9rZS1saW5lam9pbj0icm91bmQiLz4KDTxnIGlkPSJTVkdSZXBvX2ljb25DYXJyaWVyIj4gPGc+IDxwYXRoIGQ9Ik0xMzguMzU3LDBDNjIuMDY2LDAsMCw2Mi4wNjYsMCwxMzguMzU3czYyLjA2NiwxMzguMzU3LDEzOC4zNTcsMTM4LjM1N3MxMzguMzU3LTYyLjA2NiwxMzguMzU3LTEzOC4zNTcgUzIxNC42NDgsMCwxMzguMzU3LDB6IE0xMzguMzU3LDI1OC43MTVDNzEuOTkyLDI1OC43MTUsMTgsMjA0LjcyMywxOCwxMzguMzU3UzcxLjk5MiwxOCwxMzguMzU3LDE4IHMxMjAuMzU3LDUzLjk5MiwxMjAuMzU3LDEyMC4zNTdTMjA0LjcyMywyNTguNzE1LDEzOC4zNTcsMjU4LjcxNXoiLz4gPHBhdGggZD0iTTE5NC43OTgsMTYwLjkwM2MtNC4xODgtMi42NzctOS43NTMtMS40NTQtMTIuNDMyLDIuNzMyYy04LjY5NCwxMy41OTMtMjMuNTAzLDIxLjcwOC0zOS42MTQsMjEuNzA4IGMtMjUuOTA4LDAtNDYuOTg1LTIxLjA3OC00Ni45ODUtNDYuOTg2czIxLjA3Ny00Ni45ODYsNDYuOTg1LTQ2Ljk4NmMxNS42MzMsMCwzMC4yLDcuNzQ3LDM4Ljk2OCwyMC43MjMgYzIuNzgyLDQuMTE3LDguMzc1LDUuMjAxLDEyLjQ5NiwyLjQxOGM0LjExOC0yLjc4Miw1LjIwMS04LjM3NywyLjQxOC0xMi40OTZjLTEyLjExOC0xNy45MzctMzIuMjYyLTI4LjY0NS01My44ODItMjguNjQ1IGMtMzUuODMzLDAtNjQuOTg1LDI5LjE1Mi02NC45ODUsNjQuOTg2czI5LjE1Miw2NC45ODYsNjQuOTg1LDY0Ljk4NmMyMi4yODEsMCw0Mi43NTktMTEuMjE4LDU0Ljc3OC0zMC4wMDkgQzIwMC4yMDgsMTY5LjE0NywxOTguOTg1LDE2My41ODIsMTk0Ljc5OCwxNjAuOTAzeiIvPiA8L2c+IDwvZz4KDTwvc3ZnPg==" height="22">


</div>

## Overview

**Smear** is the I/O-free core of a GraphQL implementation. Today that core is a zero-copy **lexer**,
a **parser** that produces either a plain AST or a lossless CST, and a **validator** for the draft
specification's type-system and executable-document rules. Everything above the lexer is built from
reusable combinators, so the same machinery serves standard GraphQL and GraphQL-like DSLs — GraphQLx,
the extended dialect that ships alongside, is one of those DSLs rather than a special case.

**I/O-free is a design constraint, not a euphemism for unfinished.** Nothing here opens a socket,
spawns a task, or decides how you allocate; every layer is a library you call rather than a framework
that calls you. That constraint is why the parser is generic over the source representation instead
of taking `&str`, why the validator's steady state allocates nothing and keeps its scratch in a buffer
you own, and why `no_std` is reachable at all.

It is also what the rest of the plan rests on. Execution — draft §6 and §7 — is being built as a
Sans-I/O state machine: `poll_*` and `handle_*` pairs, time passed in rather than read, and the
resolved values owned by the caller behind a trait rather than copied into a `Value` enum of ours, so
a handle from FFI or wasm is a first-class value and not a conversion step. A core shaped that way can
be driven by a `tokio` server, a `compio` one, a wasm module or an editor without any of them being
privileged — which is what lets the runtime adapters and the ergonomic macro layer be thin crates
*above* the core instead of assumptions baked *into* it.

None of that is here yet, and the table below says so line by line. The direction is stated because
it explains the shape of what is here: those constraints cost something, and this is what they buy.

### What is implemented, and what is not

The specification is large, and smear covers the front half of it. Where a row below says a thing is
absent, it is absent — not partial.

| Draft section | State |
|---|---|
| §2 Language | Parsed in full, by two token streams and into two tree shapes |
| §3 Type System | Validated inside `Schema::build`: 67 distinct refusals, each with a schema in the test suite that makes it fire |
| §4 Introspection | The `__`-prefixed meta-schema is injected into every schema, so an introspection *query* is validated like any other document, and a schema can also be *built* from a server's introspection response. There is no introspection **execution** |
| §5 Validation | All 30 executable-document rules |
| §6 Execution | **Not implemented.** Smear parses and validates a request; running it is the caller's |
| §7 Response | **Not implemented.** `smear::diagnostic` does carry §7.1.2 response paths, so an executor built on top of smear can attach them |

Both counts are enumerable rather than asserted. `Rule::ALL` holds 31 entries — the 29 §5 rules that
need a runtime check, plus two non-specification resource budgets; §5.1.1 needs no entry because the
grammar gives an executable document no type-system branch to begin with. `SchemaErrorKind::ALL`
holds 67. Three floors in the test suite keep those numbers meaningful instead of decorative:
`liveness_floor` demands every rule have a document that fires it *and* a valid twin that does not,
`refusal_floor` demands the same of every schema refusal, and `branch_floor` adds 31 further rows
pinning sub-clauses that a single fixture per rule would leave unreached.

## What Smear Does That Other Rust GraphQL Parsers Do Not

Compared with `apollo-parser`, `graphql-parser`, `async-graphql-parser` and `cynic-parser`:

### One validator, reached three ways

Validation is not a separate tool bolted on after parsing. Draft §3 runs inside `Schema::build`, so a
malformed schema is refused once at startup rather than rediscovered on every request, and there is
exactly one implementation of it behind three entrances:

- **SDL** — `Schema::build`, over a parsed `TypeSystemDocument`.
- **An introspection response** — `Schema::from_introspection` renders a draft §4 response as SDL and
  hands it to the same builder.
- **A lossless CST** — `validate_schema_lossless` projects the tree and hands *that* to the same
  builder, which is what lets an editor validate a schema it is in the middle of typing.

The three do not carry identical guarantees, and the difference is worth knowing before you pick one.
An introspection response has no field for applied directives, so the directive-*usage* checks have
nothing to run on through that door — the type-structure rules run identically, the usage rules
cannot. The lossless doors recover per definition and return a `Recovery`; a caller that ignores it is
reading a partial verdict as a total one.

For executable documents, `validate_executable` and its lossless twin `validate_executable_lossless`
run the same complete §5 rule set, and `RuleSet` selects any subset of it.

### Two token streams, two tree shapes

Most Rust GraphQL parsers give you one. Smear's lexer has a `syntactic` stream that skips trivia —
whitespace, commas, comments — for servers and query execution, and a `lossless` stream that keeps
every byte of it, for formatters, linters and IDEs. The parser mirrors the split: an AST from the
first, and a rowan CST from the second, behind the `rowan` feature.

### Zero-copy, and a steady state that allocates nothing

Tokens and AST nodes hold slices into the original source; no token or node copies the text it spans.
The validator goes further: once its caller-owned `Scratch` and sink have seen a request the size of
the ones to come, validating performs **zero heap allocations**. That is measured rather than
asserted — `smear/tests/validator_allocation.rs` counts with a global allocator, and its
`the_gate_counts` test proves the counter moves, so a green reading means "nothing allocated" rather
than "nothing was looking".

### A source type you choose — on the syntactic half

The syntactic doors are generic over the source type: `&str`, `&[u8]`, `bytes::Bytes`,
`bstr::BStr`, `hipstr::{HipStr, HipByt}`, `smol_bytes::SmolBytes`, or your own. Pick a
`Send + Sync + 'static` one and the AST becomes `Send + Sync + 'static` with it, which is what makes
parallel schema compilation and batched query processing straightforward.

**This is not yet true of the whole crate.** The lossless doors and the introspection door take
`&str`, because rowan stores token text as `&str` and an introspection response is parsed from one.
A consumer holding `bytes::Bytes` can use the syntactic doors and not those. The narrowings are not
folklore: `cargo run -p source-census -- --verbose` walks the public surface and prints every one of
them with a written reason — at this commit, 24 narrowed parameters out of 663, of which 22 are
tracked against issues [#121] and [#103] as things to widen rather than accepted shapes.

### A diagnostic contract, not a `Display` string

`smear::diagnostic` is what every error family in the crate answers: a stable machine `Code`, a
`Severity`, a primary source `Location` and any number of secondary `Label`s, plus `PathSegment` for
a §7.1.2 response path. So a consumer can render into `miette`, `ariadne`, `codespan-reporting`, an
LSP diagnostic or a GraphQL error response without re-deriving structure from a formatted sentence.
It is `core` plus a span type — no allocation, no dependency, and not behind a feature.

### A kit for building GraphQL-like languages

The combinators, the generic AST nodes and the CST substrate are the reusable part, and GraphQLx is
the proof that they are: **113 syntax kinds against standard GraphQL's 87**, adding generics, `where`
clauses, type paths, map and set types, imports and wildcard specifiers. It is a different language
built on the same substrate, not GraphQL with a few extras — and it is where you would start in
building your own.

GraphQLx tracks a moving dialect specification, so its surface is **semver-exempt** until that
stabilises. It deliberately has no validator: its extensions have no specification semantics to
validate against, so checking them would be language design rather than conformance.

## Quick Start

```toml
[dependencies]
smear = "0.0.0"
```

The validator is not a default feature yet — it flips into `default` alongside a minor bump, once its
API has settled. Until then:

```toml
[dependencies]
smear = { version = "0.0.0", features = ["validator"] }
```

### Parsing

```rust
use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::ExecutableDocument,
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document},
  },
};

let document = Parser::with_parser::<
  GraphqlLexer<'_, str>,
  ExecutableDocument<&str>,
  GraphqlErrors<&str>,
  _,
  GraphQL,
>(executable_document)
.parse_str("query Hero { hero { name } }")
.expect("the query parses");

assert_eq!(document.definitions().len(), 1);
```

### Validating

Build the schema once, then validate each request against it. The `Scratch` and the `Budget` belong
to the caller and are reused across requests — that is what makes the steady state allocation-free.

```rust
# #[cfg(feature = "validator")] {
use smear::{
  lexer::tokora::{Parse as _, Parser},
  parser::graphql::{
    GraphQL,
    ast::{ExecutableDocument, TypeSystemDocument},
    error::GraphqlErrors,
    syntactic::{GraphqlLexer, executable_document, type_system_document},
  },
  validator::{Budget, First, Rule, Schema, Scratch, validate_executable},
};

let schema = Schema::build(
  &Parser::with_parser::<
    GraphqlLexer<'_, str>,
    TypeSystemDocument<&str>,
    GraphqlErrors<&str>,
    _,
    GraphQL,
  >(type_system_document)
  .parse_str("type Query { hero: Character } interface Character { name: String! }")
  .expect("the SDL parses"),
)
.expect("the SDL is a schema");

let mut scratch = Scratch::new();
let budget = Budget::default();

let request = Parser::with_parser::<
  GraphqlLexer<'_, str>,
  ExecutableDocument<&str>,
  GraphqlErrors<&str>,
  _,
  GraphQL,
>(executable_document)
.parse_str("{ hero { title } }")
.expect("the query parses");

let mut sink = First::new();
let invalid = validate_executable(&schema, &request, &mut scratch, &budget, &mut sink)
  .expect_err("`title` is not a field of `Character`");
assert_eq!(invalid.emitted(), 1);

let diagnostic = sink.get().expect("a diagnostic");
assert_eq!(diagnostic.rule(), Rule::FieldSelections);
assert_eq!(diagnostic.subject_source(), Some(&"title"));
# }
```

## Architecture

Four layers. The lower two are generic over the source type and the dialect; the upper two are
standard GraphQL only.

| Layer | Module | What it is |
|---|---|---|
| Lexer | `smear::lexer` | Source text to zero-copy tokens, in a syntactic or a lossless stream. The irreducible base — it has no feature of its own because the parser cannot exist without it |
| Parser | `smear::parser` | Combinators that build an AST from the syntactic stream, and the rowan CST tower from the lossless one, plus the generic node definitions a new dialect reuses |
| Validator | `smear::validator` | The built-once `Schema`, draft §3 inside its build, and the draft §5 rules over a parsed request |
| Diagnostic | `smear::diagnostic` | The contract every error family above answers, so rendering is the consumer's choice |

## Feature Flags

Fourteen features. The lexer is not one of them: it is the irreducible base, the parser cannot exist
without it, and a gate that can only ever be on is not a gate.

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support; off is `no_std`, and `alloc` is required either way | ✓ |
| `graphql` | Standard GraphQL, in every layer | ✓ |
| `graphqlx` | Extended GraphQL, in the lexer and parser. Semver-exempt until the dialect stabilises | ✓ |
| `parser` | `smear::parser` — the combinators and the ASTs. Off, the crate is the lexer alone | ✓ |
| `smallvec` | Use `smallvec` for small collections | ✓ |
| `validator` | `smear::validator` — the built-once `Schema`, draft §3 inside its build, and the draft §5 rules. Implies `parser` and `graphql`. Adds no dependency | |
| `introspection` | `Schema::from_introspection`, building a schema from a draft §4 response. Implies `validator` and `std`, and is the one validator feature that costs a dependency (`serde`, `serde_json`) | |
| `rowan` | The lossless CST tower, and with `validator` the lossless validation doors. Implies `parser` and `std` | |
| `bytes` | Support the `bytes::Bytes` source type | |
| `bstr` | Support the `bstr::BStr` source type | |
| `hipstr` | Support the `hipstr::{HipStr, HipByt}` source types | |
| `smol-bytes` | Support the `smol_bytes::SmolBytes` source type | |
| `lossless-coverage` | Per-node-kind hit counters for the lossless gates. Implies `rowan` | |
| `test-support` | The lossless suites' `test_support` scaffolding | |

A lexer-only consumer — a syntax highlighter, a formatter front-end, token-level tooling — turns the
parser off:

```toml
[dependencies]
smear = { version = "0.0.0", default-features = false, features = ["std", "graphql", "smallvec"] }
```

### `no_std`

With `std` off the crate is `no_std` and requires `alloc`. CI cross-compiles `smear` with
`--all-features` for fourteen targets including five WebAssembly ones, and builds the validator's
schema representation for `thumbv6m-none-eabi` — a core with no compare-and-swap — through the
`smear-noatomic` member, which `#[path]`-includes smear's own files so the proof cannot drift from
the source.

Two limits on that claim, because they are the difference between "compiles" and "works". `smear`
itself does **not** build for `thumbv6m-none-eabi`: its AST offers an `Arc`-backed list spelling that
needs native atomics, which is why the no-atomic proof is about the schema representation rather than
the crate. And per [#124], the `not(std)` arm is compile-checked but **executed** by nothing — the
crate's dev-dependency on itself does not pass `default-features = false`, so every test build
resolves `std` back on. Treat `no_std` as a supported build, not as a tested runtime.

## Benchmarks

Two packages carry the benchmark suites. Every target sets `harness = false` and runs under criterion,
so a run must name its target:

```sh
cargo bench --package smear-benches      --bench executables          -- --quick
cargo bench --package smear-benches      --bench type_system          -- --quick
cargo bench --package smear-apollo-bench --bench validator_comparison -- --quick
cargo bench --package smear-apollo-bench --bench apollo_comparison    -- --quick
```

`smear-benches` measures parsing against four other Rust GraphQL parsers, and `smear-apollo-bench`
measures the lossless CST tower and draft §5 validation against `apollo-parser` and
`apollo-compiler`.

**No tables here.** The ones this file used to carry were measured on an unrecorded commit, date and
machine, and had drifted far enough to understate the crate — so they were removed rather than
annotated, because a stale table with a caveat above it still reads as data. They go back when there
is a run worth publishing, and a run is worth publishing when it records the commit, the date, the
machine and the exact invocation, taken on a host verified idle before and after. Note that macOS
load average is not a usable signal for that — it reads around 3.3 at 80% idle.

## Who Should Use Smear?

- GraphQL tooling — IDEs, linters, formatters — which want the lossless tree and the recovering doors
- GraphQL servers, which want the syntactic tree, the built-once schema and the allocation-free
  steady state
- Schema analysis and validation tools
- Anyone building a GraphQL-like DSL, who wants the combinators and the CST substrate rather than a
  fixed grammar

Smear is **not** a GraphQL server: there is no execution engine and no response serialisation. It is
the front end one would be built on.

Migration note: `smear-lexer` and `smear-parser` were merged into this crate in [#83]. Neither had
ever been published, so nothing on crates.io moved; path and git dependents rename `smear_lexer::X`
to `smear::lexer::X` and `smear_parser::X` to `smear::parser::X`, and select features per the table
above.

## Contributing

Contributions are welcome! Please feel free to submit issues or pull requests.

## License

<sup>
Licensed under either of <a href="https://opensource.org/licenses/Apache-2.0">Apache License, Version
2.0</a> or <a href="https://opensource.org/licenses/MIT">MIT license</a> at your option.
</sup>

<br>

<sub>
Unless you explicitly state otherwise, any contribution intentionally submitted
for inclusion in this project by you, as defined in the Apache-2.0 license,
shall be dual licensed as above, without any additional terms or conditions.
</sub>

[Github-url]: https://github.com/al8n/smear/
[CI-url]: https://github.com/al8n/smear/actions/workflows/ci.yml
[codecov-url]: https://app.codecov.io/gh/al8n/smear
[doc-url]: https://docs.rs/smear
[crates-url]: https://crates.io/crates/smear
[#83]: https://github.com/al8n/smear/pull/83
[#103]: https://github.com/al8n/smear/issues/103
[#121]: https://github.com/al8n/smear/issues/121
[#124]: https://github.com/al8n/smear/issues/124
