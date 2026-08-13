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

**Smear** is a next-generation GraphQL implementation for Rust, built I/O-free from the bottom up.
Today that means a zero-copy **lexer**, a **parser** that produces either a plain AST or a lossless
CST, and a **validator** for the draft specification's type-system and executable-document rules.
Everything above the lexer is built from reusable combinators, so the same machinery serves standard
GraphQL and GraphQL-like DSLs — GraphQLx, the extended dialect that ships alongside, is one of those
DSLs rather than a special case.

*Next generation* is a claim about what smear does **not** decide for you. Today's Rust GraphQL
libraries each settle three things on their users' behalf: the **string type**, because the parser's
entry point takes `&str`; the **control flow**, because you register resolvers and an executor calls
them; and the **shape of a resolved value**, because the executor defines a `Value` enum to hold it.
Each of those is a reasonable default, and each of them is the kind of default you cannot change
afterwards. Smear declines all three.

**I/O-free is the mechanism, not a euphemism for unfinished.** Nothing here opens a socket, spawns a
task, or decides how you allocate; every layer is a library you call rather than a framework that
calls you. That constraint is why the parser is generic over the source representation instead of
taking `&str`, why the validator's steady state allocates nothing and keeps its scratch in a buffer
you own, and why `no_std` is reachable at all.

The same constraint shapes what comes next. Execution — draft §6 and §7 — is being built as a
Sans-I/O state machine: `poll_*` and `handle_*` pairs, time passed in rather than read, and the
resolved values owned by the caller behind a trait rather than copied into a `Value` enum of ours, so
a handle from FFI or wasm is a first-class value and not a conversion step. A core shaped that way can
be driven by a `tokio` server, a `compio` one, a wasm module or an editor without any of them being
privileged — which is what lets the runtime adapters and the ergonomic macro layer be thin crates
*above* the core instead of assumptions baked *into* it.

The first phase of that is here, behind the non-default `proto` feature: query execution, and
nothing above it. The table below is the specification coverage as it stands, and the
[roadmap](#roadmap) is the list of what is still missing, unchecked.

### What is implemented, and what is not

The specification is large, and smear covers the front half of it. Where a row below says a thing is
absent, it is absent — not partial.

| Draft section | State |
|---|---|
| §2 Language | Parsed in full, by two token streams and into two tree shapes |
| §3 Type System | Validated inside `Schema::build`: 67 distinct refusals, each with a schema in the test suite that makes it fire |
| §4 Introspection | The `__`-prefixed meta-schema is injected into every schema, so an introspection *query* is validated like any other document, and a schema can also be *built* from a server's introspection response. There is no introspection **execution** |
| §5 Validation | All 30 executable-document rules |
| §6 Execution | **All three operations, behind the non-default `proto` feature.** `smear::proto`'s Sans-I/O executor does collect, coerce arguments, complete, resolve abstract types and propagate nulls. §6.2.2's serial mutation is structural rather than a contract — the top-level fields are withheld from `poll_resolve` rather than queued — and §6.2.3's subscription is a third value of the same at-most-one operation-kind phase, not a second type: `start` runs §6.2.3.1 and hands the driver the one source field to resolve into an event stream, each event it pushes back is one whole execution, and §6.2.3.2's one-result-per-event ordering is owned by the intake. What is still absent is the transport: the crate owns no stream, no clock and no timers, so the connection state machine and backpressure a long-lived operation needs live in a driver |
| §7 Response | **The tree, not the document.** A finished execution hands back the response as a walkable tree, and its field errors carry §7.1.2's `path` and `locations`; `smear::diagnostic` carries the same response paths for a consumer that has no executor. All three of §7.1's result kinds are modelled — §7.1.1's execution result, §7.1.3's request error result as a distinct type with no `data` accessor at all, and §7.1.2's response stream as the state of a running subscription rather than a container, because a crate that owns no stream cannot hold one. The response map's third entry is there too — the driver attaches a §7.1.7 *Extensions* map, a container that cannot hold a non-map, cannot grow past `max_extension_entries` or `max_extension_key_bytes`, and cannot be attached outside the one phase a response could carry it in; the executor hands the values back unread, at both of §7.1.7's sites. One `extensions` site is deliberately absent: the per-*error* one of §7.1.6's error result format, because it would decide part of the diagnostic contract. Nothing writes `data`, `errors` and `extensions` out, because serialising a driver's leaf is the driver's |

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
`&str`, because rowan stores token text as `&str` and an introspection response is parsed from one;
so does the one method of `proto`'s value trait that hands a driver the name of a variable. A
consumer holding `bytes::Bytes` can use the syntactic doors and not those. The narrowings are not
folklore: `cargo run -p source-census -- --verbose` walks the public surface and prints every one of
them with a written reason — at this commit, 25 narrowed parameters out of 687, of which 23 are
tracked against issues [#121], [#103] and [#139] as things to widen rather than accepted shapes.

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

Five layers, one crate each, and `smear` is an umbrella that re-exports all of them. The lower two
are generic over the source type and the dialect; the upper three are standard GraphQL only.

| Layer | Crate | Reached as | What it is |
|---|---|---|---|
| Lexer | `smear-lexer` | `smear::lexer` | Source text to zero-copy tokens, in a syntactic or a lossless stream. The irreducible base — it has no feature of its own because everything above it is built on its token streams |
| Parser | `smear-parser` | `smear::parser` | Combinators that build an AST from the syntactic stream, and the rowan CST tower from the lossless one, plus the generic node definitions a new dialect reuses |
| Schema | `smear-schema` | `smear::validator::schema` | The built-once `Schema` and, behind its `build` feature, draft §3 inside the builder. A consumer that only *reads* a schema takes it with `default-features = false` and gets no front end |
| Validator | `smear-compiler` | `smear::validator` | The draft §5 rules over a parsed request, the rule vocabulary and the diagnostic rendering |
| Execution | `graphql-proto` | `smear::proto` | Draft §6 as a Sans-I/O state machine: it says which field it needs next and waits to be told the answer, so the driver keeps the values, the resolvers and the runtime. Queries today, behind the non-default `proto` feature |
| Diagnostic | (in `smear-schema`) | `smear::diagnostic` | The contract every error family above answers, so rendering is the consumer's choice. It ships with the schema because that is the lowest crate whose errors implement it |

**Depend on `smear` and enable features, or depend on a member directly** — the two are the same
code and the same paths. `smear::lexer::X` and `smear_lexer::X` are one item. A member is worth
naming directly when you want exactly one layer: a syntax highlighter wants `smear-lexer` and never
resolves the parser tower; a driver that reads a schema at execution time wants `smear-schema` with
`default-features = false` and never resolves the front end. Every feature a member declares is
forwarded by `smear` under the same name, which `ci/feature_reachability.py` checks by enumerating
the members rather than by reading the umbrella's own table.

## Feature Flags

Fifteen features. The lexer is not one of them: it is the irreducible base, the parser cannot exist
without it, and a gate that can only ever be on is not a gate.

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support; off is `no_std`, and `alloc` is required either way | ✓ |
| `graphql` | Standard GraphQL, in every layer | ✓ |
| `graphqlx` | Extended GraphQL, in the lexer and parser. Semver-exempt until the dialect stabilises | ✓ |
| `parser` | `smear::parser` — the combinators and the ASTs. Off, `smear-parser` is not in the graph at all and the umbrella is the lexer alone | ✓ |
| `smallvec` | Use `smallvec` for small collections | ✓ |
| `validator` | `smear::validator` — the draft §5 rules, plus `smear-schema`'s `build` feature for the §3 pass. Implies `parser` and `graphql`. Adds no third-party dependency | |
| `proto` | `smear::proto` — draft §6 query execution as a Sans-I/O state machine. Implies `validator`, because execution is entered with a document the §5 rules have already accepted. Adds no third-party dependency and defines no value type: the driver's own representation reaches it through a trait | |
| `introspection` | `Schema::from_introspection`, building a schema from a draft §4 response. Implies `validator` and `std`. Adds no third-party dependency | |
| `materialized-numbers` | A second set of GraphQL value productions whose `Int` and `Float` leaves carry `i64` and `f64` instead of a source slice. **Numbers only** — strings keep their slice, because materialisation is required to allocate nothing the slice parser did not already allocate. Implies `parser` and `graphql`; adds no dependency, leaves the slice AST and the slice parser untouched, and keeps the slice parser available for the literals `i64` cannot hold | |
| `rowan` | The lossless CST tower, and with `validator` the lossless validation doors. Implies `parser` and `std` | |
| `bytes` | Support the `bytes::Bytes` source type | |
| `bstr` | Support the `bstr::BStr` source type | |
| `hipstr` | Support the `hipstr::{HipStr, HipByt}` source types | |
| `smol-bytes` | Support the `smol_bytes::SmolBytes` source type | |
| `lossless-coverage` | Per-node-kind hit counters for the lossless gates. Implies `rowan` | |
| `test-support` | The lossless suites' `test_support` scaffolding | |

A lexer-only consumer — a syntax highlighter, a formatter front-end, token-level tooling — turns the
parser off, or names the member and skips the umbrella entirely. The two resolve the same code:

```toml
[dependencies]
smear = { version = "0.0.0", default-features = false, features = ["std", "graphql", "smallvec"] }
# or
smear-lexer = { version = "0.0.0", default-features = false, features = ["std", "graphql", "smallvec"] }
```

### `no_std`

With `std` off the crate is `no_std` and requires `alloc`. CI cross-compiles `smear` with
`--all-features` for fourteen targets including five WebAssembly ones, and builds the validator's
schema representation for `thumbv6m-none-eabi` — a core with no compare-and-swap — through the
`smear-noatomic` member, which `#[path]`-includes `smear-schema`'s own files so the proof cannot
drift from the source.

Two limits on that claim, because they are the difference between "compiles" and "works". `smear`
itself does **not** build for `thumbv6m-none-eabi`: its AST offers an `Arc`-backed list spelling that
needs native atomics, which is why the no-atomic proof is about the schema representation rather than
the crate. And per [#124], the `not(std)` arm is compile-checked but **executed** by nothing: the one
row that runs the suite is `cargo test --all-features`, which has `std` on. Until [#136] a test build
could not even *compile* it — the crate's dev-dependency on itself omitted
`default-features = false`, so a `--no-default-features` selection resolved `std` back on, and every
narrowed row was really the default build wearing a narrower name. Treat `no_std` as a supported
build, not as a tested runtime.

## Benchmarks

All six live in `smear/benches`, as `smear`'s own `[[bench]]` targets, so `cargo bench` is the whole
suite. They are grouped by how many implementations are inside the timed region:

* `benches/solo/` — `lex_baseline` and `graphqlx`, where both sides are smear. The number is
  comparable only to an earlier run on the same machine.
* `benches/comparative/` — `executables` and `type_system` against four other Rust GraphQL parsers,
  and `benches/comparative/apollo/`'s `apollo_comparison` and `validator_comparison` against
  `apollo-parser` and `apollo-compiler`. The number is a ratio.

Every target sets `harness = false` and runs under criterion, so a run must name its target. The two
apollo ones additionally need the features they measure:

```sh
cargo bench -p smear --bench executables -- --quick
cargo bench -p smear --bench type_system -- --quick
cargo bench -p smear --features rowan,validator --bench apollo_comparison    -- --quick
cargo bench -p smear --features rowan,validator --bench validator_comparison -- --quick
```

Those two carry `required-features`, and an unmet `required-features` makes cargo *skip* a target in
silence. `cargo bench -p smear` therefore builds four of the six and exits 0; the CI step that closes
that names both targets explicitly.

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

Smear is **not** a GraphQL server *yet*: there is no execution engine and no response serialisation.
Today it is the front end one would be built on; the [roadmap](#roadmap) is the rest.

Migration note: `smear-lexer` and `smear-parser` were merged into this crate in [#83] and are
separate crates again, joined by `smear-schema`, `smear-compiler` and `graphql-proto`. **Nothing a
consumer writes changed in either direction**: `smear` re-exports every member under the module
name it had, so `smear::lexer::X`, `smear::parser::X`, `smear::validator::X`, `smear::proto::X` and
`smear::diagnostic::X` all still resolve, and every feature is forwarded under the same name. What
the split adds is the option of depending on one layer directly — see [Architecture](#architecture).

## Roadmap

Every box below is unchecked, and unchecked means absent — not partial, not planned-and-half-landed.
The first two carry an inline exception, because execution's first phase has landed and a box that
stayed silent about it would be the reading this sentence exists to forbid. The list is what stands
between the current front end and a complete implementation.

- [ ] **§6 Execution** — the Sans-I/O engine described in the Overview. All three of §6.2's
  operations execute behind the non-default `proto` feature — query, serial mutation, and
  subscription as a phase of the same machine. What has not landed is everything a *transport*
  needs: the connection state machine, backpressure, and the timers a long-lived operation wants.
  That is why the box is unchecked.
- [ ] **§7 Response** — assembling and serialising the result. Execution hands back a response tree,
  and its field errors carry §7.1.2's `path` and `locations` the same way `smear::diagnostic` does;
  all three of §7.1's result kinds are modelled, and both of §7.1.7's *Extensions* sites are
  carried. The per-*error* `extensions` of §7.1.6 is not, and nothing writes `data`, `errors` and
  `extensions` out as a document.
- [ ] **Introspection execution** — a schema can be *built* from an introspection response, and an
  introspection query is *validated* like any other document, because the meta-schema is injected
  into every schema. Nothing here **answers** `__schema` or `__type`.
- [ ] **Runtime adapters** — separate crates that drive the core from a `tokio` server, a `compio`
  one, or a wasm host. Thin is the point: each one is a driver, and the core stays runtime-free.
- [ ] **Macro layer** — deriving schema types and resolver wiring from Rust types, so the ergonomic
  surface sits *above* the core instead of being the only way in.
- [ ] **`graphql-transport-ws`** — the subscription transport. Execution now produces the stream
  §7.1.2 describes, one execution result per source event, so what is left is the wire protocol and
  the connection lifecycle around it.
- [ ] **Diagnostic rendering** — smear ships the contract and deliberately no renderer, so nothing
  here turns an error into a rendered snippet. A companion crate for that is being built
  separately.

No dates. The order above is roughly the order in which each item unblocks the next.

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
[#136]: https://github.com/al8n/smear/issues/136
[#139]: https://github.com/al8n/smear/issues/139
