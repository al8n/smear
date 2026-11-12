# `smear-lexer`

[![Crates.io](https://img.shields.io/crates/v/smear-lexer.svg)](https://crates.io/crates/smear-lexer)
[![Documentation](https://docs.rs/smear-lexer/badge.svg)](https://docs.rs/smear-lexer)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

Blazing fast, zero-copy lexer for GraphQL and GraphQL-like DSLs.

`smear-lexer` is the lexical analysis layer of [Smear](https://github.com/al8n/smear), providing high-performance tokenization for GraphQL and GraphQLx source text. It is the stack's irreducible base: everything above it is built on the token streams below, and it carries no feature that switches itself off.

## Features

- **True Zero-Copy**: All tokens hold slices into the original source—no string allocations
- **Dual Token Streams**: Choose `SyntacticToken` (fast, skips trivia) for servers or `LosslessToken` (complete, preserves formatting) for tooling
- **Generic Over Source Types**: Works with `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::{HipStr, HipByt}`, smol-bytes 0.2's `Bytes` and `Utf8Bytes`, and custom types
- **Draft Spec Compliant**: Fully implements GraphQL draft specification lexical grammar
- **GraphQLx Support**: Extended tokens for generics, `use` items, namespacing, and more
- **`no_std` Compatible**: Two independent tiers, `std` and `alloc`. With neither the crate still compiles and keeps `hints`, `keywords`, `limits` and `punctuator`; the lexers need `std` or `alloc`
- **Comprehensive Error Reporting**: Detailed, actionable error messages with source location tracking

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
# The lexer alone. For the parser tower, the validator or the protocol machine on top of it,
# depend on `smear` and enable the matching feature there.
smear-lexer = { version = "0.0.1", default-features = false, features = ["std", "graphql", "smallvec_1"] }
```

### Basic Usage

```rust,ignore
use smear_lexer::{graphql::syntactic::Lexer, logosky::Lexed};

let source = "query { user { id name } }";
let tokens = Lexer::<&str>::new(source);

for token in tokens {
    match token {
        Lexed::Token(tok) => println!("{:?}", tok),
        Lexed::Error(e) => eprintln!("Lexer error: {:?}", e),
    }
}
```

## Token Streams: Syntactic vs Lossless

`smear-lexer` provides two complementary token types for different use cases:

### SyntacticToken (Fast)

**Use for**: GraphQL servers, query execution, performance-critical parsing

- Automatically **skips trivia** (whitespace, comments, commas)
- Minimal memory footprint
- Maximum speed
- Zero-copy—all tokens reference original source

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;

let source = "query GetUser { user(id: 123) { name } }";
let tokens = Lexer::<&str>::new(source);
// Only syntactically significant tokens (whitespace automatically skipped)
```

### LosslessToken (Complete)

**Use for**: Code formatters, linters, IDEs, syntax highlighters, refactoring tools

- **Preserves all trivia** (whitespace, comments, commas, exact formatting)
- Every character from source is represented
- Perfect for building Concrete Syntax Trees (CST)
- Enables accurate source reconstruction

```rust,ignore
use smear_lexer::graphql::lossless::Lexer;

let source = "query GetUser { user(id: 123) { name } }";
let tokens = Lexer::<&str>::new(source);
// ALL tokens including spaces, comments, exact formatting
```

## Supported Source Types

The lexer is generic over source type `S`, enabling flexibility:

### String Slices (`&str`)

Most common, for text that is already UTF-8. The type guarantees it, so the lexer checks nothing again:

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use logosky::TokenStream;

let source = "{ field }";
let tokens = Lexer::<&str>::new(source);
```

### Byte Slices (`&[u8]`)

For binary protocols, or input not yet known to be UTF-8. A byte source is not validated as UTF-8 wholesale, but its string literals and comments must be well-formed UTF-8: an inline or block string literal that is not is refused with `StringError::UnsupportedCharacter`, a comment with `LexerErrorData::UnknownLexeme`, one error per ill-formed sequence, at that sequence's span. `bytes::Bytes`, `bstr::BStr`, `hipstr::HipByt` and smol-bytes' two `Bytes` are byte sources too and follow the same rule.

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;

let source = b"{ field }";
let tokens = Lexer::<&[u8]>::new(&source[..]);
```

### Shared Ownership (`bytes::Bytes`)

Enable the `bytes_1` feature for cheap cloning with Arc-backed storage:

```toml
[dependencies]
smear-lexer = { version = "0.0.1", default-features = false, features = ["std", "graphql", "smallvec_1", "bytes_1"] }
```

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use bytes::Bytes;

let source = Bytes::from("{ field }");
let tokens = Lexer::<Bytes>::new(source);
// Tokens can be cloned cheaply (Arc-backed)
```

### `hipstr` Types

Enable the `hipstr_0_8` feature for inline-optimized strings:

```toml
[dependencies]
smear-lexer = { version = "0.0.1", default-features = false, features = ["std", "graphql", "smallvec_1", "hipstr_0_8"] }
```

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use hipstr::HipStr;

let source = HipStr::from("{ field }");
let tokens = Lexer::<HipStr>::new(source);
```

### `smol-bytes` Types

Enable the `smol-bytes_0_2` feature for smol-bytes 0.2's inline-optimized buffers, `Bytes` and `Utf8Bytes`, each in a shared and a compact form (`smol_bytes::shared::Bytes`, `smol_bytes::compact::Bytes`, `smol_bytes::Utf8Bytes`, `smol_bytes::compact::Utf8Bytes`). The two `Utf8Bytes` are text sources; the two `Bytes` are byte sources and follow the UTF-8 rule above.

```toml
[dependencies]
smear-lexer = { version = "0.0.1", default-features = false, features = ["std", "graphql", "smallvec_1", "smol-bytes_0_2"] }
```

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use smol_bytes::Utf8Bytes;

let source = Utf8Bytes::from("{ field }");
let tokens = Lexer::<Utf8Bytes>::new(source);
```

## GraphQL Token Types

The GraphQL lexer recognizes:

- **Identifiers**: Names for types, fields, arguments (`User`, `firstName`, `id`)
- **Literals**:
  - Integers: `42`, `-10`, `0`
  - Floats: `3.14`, `-0.5`, `1e10`, `2.5E-3`
  - Strings: `"hello"` (inline), `"""block"""` (block)
  - Booleans: `true`, `false`
  - Null: `null`
- **Punctuators**: `(` `)` `{` `}` `[` `]` `:` `=` `@` `$` `!` `|` `&` `...` `,`
- **Keywords**: `query`, `mutation`, `subscription`, `fragment`, `type`, `interface`, `union`, `enum`, `input`, `schema`, `extend`, `scalar`, `implements`, `directive`, `on`, `repeatable`
- **Trivia** (LosslessToken only): Whitespace, line terminators, comments, commas

## GraphQLx: Extended GraphQL

Enable the `graphqlx` feature for extended tokens:

```toml
[dependencies]
smear-lexer = { version = "0.0.1", default-features = false, features = ["std", "graphqlx", "smallvec_1"] }
```

GraphQLx is a **superset of GraphQL**, meaning it includes all standard GraphQL tokens plus additional extensions:

- **Literals**:
  - **Integers** (all GraphQL formats plus):
    - Hexadecimal: `0xFF`, `0x1A2B`, `-0xDEADBEEF`
    - Binary: `0b1010`, `0b11111111`, `-0b101`
    - Octal: `0o755`, `0o644`, `-0o777`
    - Decimal (same as GraphQL): `42`, `-10`, `0`
  - **Floats** (all GraphQL formats plus):
    - Hexadecimal: `0x1.8p3`, `0xA.Bp-2`, `-0x1.FFFFFEp127`
    - Decimal (same as GraphQL): `3.14`, `-0.5`, `1e10`, `2.5E-3`
- **Punctuators**: `<` `>` `::` `=>` `*` `+` `-` `~` `^` `..`, where `..` is taken by longest match after `...`, so `....` is `...` then a stray `.`

    ```rust,ignore
    use smear_lexer::graphqlx::syntactic::Lexer;

    let source = r#"use "./types"::User"#;
    let tokens = Lexer::<&str>::new(source);
    ```

## Error Handling

The lexer provides comprehensive error reporting:

```rust,ignore
use smear_lexer::{graphql::syntactic::Lexer, logosky::Lexed};

let source = "query { 0123 }"; // Leading zeros not allowed
let mut tokens = Lexer::<&str>::new(source);

match tokens.into_iter().next() {
    Some(Lexed::Error(errors)) => {
        for error in errors.iter() {
            println!("Error at {:?}: {:?}", error.span(), error.data());
        }
    }
    _ => {}
}
```

Common error types:

- **Invalid number literals**: Leading zeros, invalid suffixes, malformed floats
- **Unterminated strings**: Missing closing quotes
- **Invalid escape sequences**: Unknown escape characters in strings
- **Invalid Unicode**: Malformed Unicode escape sequences, unpaired surrogates
- **Unknown characters**: Characters not recognized by the lexer

## Feature Flags

Every feature this crate declares. `smear` forwards each of them under the same name, so a
consumer of the umbrella spells the same word; see `smear`'s own table for the features that
belong to the layers above (`parser`, `validator`, `proto`, `rowan_0_17`, …).

| Feature | Description | Default |
|---------|-------------|---------|
| `alloc` | The allocating API without the standard library: the dialect lexers, the lossless lexer, the error types and the string lexers. `std` gives the same API; with neither, the crate is `no_std` and keeps only `hints`, `keywords` (with `keyword!`), `limits` and `punctuator`. Implied by no feature | |
| `std` | Standard library support, and with it the allocating API; it does not turn `alloc` on. Off, the crate is `no_std` | ✓ |
| `graphql` | Standard GraphQL lexer. Needs `std` or `alloc` | ✓ |
| `graphqlx` | Extended GraphQL lexer; semver-exempt while the dialect moves. Needs `std` or `alloc` | ✓ |
| `smallvec_1` | Use `smallvec` 1 for small collections. Implies no tier | ✓ |
| `bytes_1` | Support `bytes::Bytes` source type, from `bytes` 1. Implies no tier | |
| `bstr_1` | Support `bstr::BStr` source type, from `bstr` 1. Implies no tier | |
| `hipstr_0_8` | Support `hipstr::{HipStr, HipByt}` source type, from `hipstr` 0.8. Implies no tier | |
| `smol-bytes_0_2` | Support smol-bytes 0.2 `smol_bytes::{Bytes, Utf8Bytes}` and `smol_bytes::compact::{Bytes, Utf8Bytes}` source types. Implies no tier | |

No feature but `default` switches a tier on, neither a capability (`graphql`, `graphqlx`) nor a
dependency feature, so a `default-features = false` consumer names `std` or `alloc` itself.

The five dependency features carry the major of the crate they enable, and the dependency is named
the same way, so a second major of one of them can be added beside it. The unversioned names —
`smallvec`, `bytes`, `bstr`, `hipstr` and `smol-bytes` — are aliases for the current version: each
enables its versioned feature and nothing else.

## Performance Characteristics

- **Zero allocations**: All tokens are slices into original source
- **Lazy evaluation**: Tokens generated on-demand via iterators
- **Cache-friendly**: Sequential memory access patterns
- **SIMD-ready**: Underlying lexer uses vectorized operations where beneficial

## Part of the Smear Ecosystem

`smear-lexer` is the tokenization layer of [Smear](https://github.com/al8n/smear), the bottom of
a stack whose upper layers are separate crates:

- `smear-lexer` — lexical analysis (this crate)
- `smear-parser` — parser combinators, ASTs and the lossless CST tower
- `smear-schema` — the built-once schema representation and its builder
- `smear-compiler` — draft §5 validation and diagnostics
- `graphql-proto` — the Sans-I/O protocol machine
- `smear` — the umbrella that re-exports all of them, each behind a feature

(Plain code spans and not intra-doc links: nothing above this crate is in its dependency graph,
so a link to one would be an `unresolved link` error under `-D warnings`.)

A consumer of the umbrella reaches this crate as `smear::lexer`, which is the path it had while
the two were one crate (#83), so nothing written against that spelling has to move.

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
