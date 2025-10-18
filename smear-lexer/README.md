# smear-lexer

[![Crates.io](https://img.shields.io/crates/v/smear-lexer.svg)](https://crates.io/crates/smear-lexer)
[![Documentation](https://docs.rs/smear-lexer/badge.svg)](https://docs.rs/smear-lexer)
[![License](https://img.shields.io/badge/License-Apache%202.0/MIT-blue.svg)](https://github.com/al8n/smear)

Blazing fast, zero-copy lexer for GraphQL and GraphQL-like DSLs.

`smear-lexer` is the lexical analysis layer of the [Smear](https://github.com/al8n/smear) parser ecosystem, providing high-performance tokenization for GraphQL and GraphQLX source text.

## Features

- **True Zero-Copy**: All tokens hold slices into the original source—no string allocations
- **Dual Token Streams**: Choose `SyntacticToken` (fast, skips trivia) for servers or `LosslessToken` (complete, preserves formatting) for tooling
- **Generic Over Source Types**: Works with `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::{HipStr, HipByt}`, and custom types
- **Draft Spec Compliant**: Fully implements GraphQL draft specification lexical grammar
- **GraphQLX Support**: Extended tokens for generics, imports, namespacing, and more
- **`no_std` Compatible**: Works in embedded environments with optional `alloc` support
- **Comprehensive Error Reporting**: Detailed, actionable error messages with source location tracking

## Quick Start

Add to your `Cargo.toml`:

```toml
[dependencies]
smear-lexer = "0.0.0"
```

### Basic Usage

```rust
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

```rust
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

```rust
use smear_lexer::graphql::lossless::Lexer;

let source = "query GetUser { user(id: 123) { name } }";
let tokens = Lexer::<&str>::new(source);
// ALL tokens including spaces, comments, exact formatting
```

## Supported Source Types

The lexer is generic over source type `S`, enabling flexibility:

### String Slices (`&str`)

Most common for UTF-8 validated text:

```rust
use smear_lexer::graphql::syntactic::Lexer;
use logosky::TokenStream;

let source = "{ field }";
let tokens = Lexer::<&str>::new(source);
```

### Byte Slices (`&[u8]`)

For binary protocols or pre-validation:

```rust
use smear_lexer::graphql::syntactic::Lexer;

let source = b"{ field }";
let tokens = Lexer::<&[u8]>::new(&source[..]);
```

### Shared Ownership (`bytes::Bytes`)

Enable the `bytes` feature for cheap cloning with Arc-backed storage:

```toml
[dependencies]
smear-lexer = { version = "0.0.0", features = ["bytes"] }
```

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use bytes::Bytes;

let source = Bytes::from("{ field }");
let tokens = Lexer::<Bytes>::new(source);
// Tokens can be cloned cheaply (Arc-backed)
```

### `hipstr` Types

Enable the `hipstr` feature for inline-optimized strings:

```toml
[dependencies]
smear-lexer = { version = "0.0.0", features = ["hipstr"] }
```

```rust,ignore
use smear_lexer::graphql::syntactic::Lexer;
use hipstr::HipStr;

let source = HipStr::from("{ field }");
let tokens = Lexer::<HipStr>::new(source);
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

## GraphQLX: Extended GraphQL

Enable the `graphqlx` feature (requires `unstable`) for extended tokens:

```toml
[dependencies]
smear-lexer = { version = "0.0.0", features = ["graphqlx", "unstable"] }
```

GraphQLX is a **superset of GraphQL**, meaning it includes all standard GraphQL tokens plus additional extensions:

- **Literals**:
  - **Integers** (all GraphQL formats plus):
    - Hexadecimal: `0xFF`, `0x1A2B`, `-0xDEADBEEF`
    - Binary: `0b1010`, `0b11111111`, `-0b101`
    - Octal: `0o755`, `0o644`, `-0o777`
    - Decimal (same as GraphQL): `42`, `-10`, `0`
  - **Floats** (all GraphQL formats plus):
    - Hexadecimal: `0x1.8p3`, `0xA.Bp-2`, `-0x1.FFFFFEp127`
    - Decimal (same as GraphQL): `3.14`, `-0.5`, `1e10`, `2.5E-3`
- **Punctuators**: `<` `>` `::` `=>` `*` `+` `-`

    ```rust
    use smear_lexer::graphqlx::syntactic::Lexer;

    let source = r#"import { User } from "./types.graphqlx""#;
    let tokens = Lexer::<&str>::new(source);
    ```

## Error Handling

The lexer provides comprehensive error reporting:

```rust
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

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support | ✓ |
| `alloc` | Allocation support for `no_std` | |
| `graphql` | Standard GraphQL lexer | ✓ |
| `graphqlx` | Extended GraphQL lexer (requires `unstable`) | ✓ |
| `unstable` | Unstable features | |
| `smallvec` | Use `smallvec` for small collections | ✓ |
| `bytes` | Support `bytes::Bytes` source type | |
| `bstr` | Support `bstr::BStr` source type | |
| `hipstr` | Support `hipstr::{HipStr, HipByt}` source type | |

## Performance Characteristics

- **Zero allocations**: All tokens are slices into original source
- **Lazy evaluation**: Tokens generated on-demand via iterators
- **Cache-friendly**: Sequential memory access patterns
- **SIMD-ready**: Underlying lexer uses vectorized operations where beneficial

## Part of the Smear Ecosystem

`smear-lexer` is the tokenization layer of [Smear](https://github.com/al8n/smear), a complete GraphQL parsing solution:

- **[smear-lexer](https://crates.io/crates/smear-lexer)**: Lexical analysis (this crate)
- **[smear](https://crates.io/crates/smear)**: Complete parser with AST construction

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](http://www.apache.org/licenses/LICENSE-2.0))
- MIT license ([LICENSE-MIT](http://opensource.org/licenses/MIT))

at your option.

### Contribution

Unless you explicitly state otherwise, any contribution intentionally submitted for inclusion in the work by you, as defined in the Apache-2.0 license, shall be dual licensed as above, without any additional terms or conditions.
