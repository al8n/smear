# smear-parser

High-performance parser for GraphQL and GraphQL-like DSLs.

## Overview

`smear-parser` provides zero-copy parser combinators for GraphQL and GraphQL-like DSLs. Built on top of [`smear-scaffold`](https://crates.io/crates/smear-scaffold) and [`smear-lexer`](https://crates.io/crates/smear-lexer), it delivers blazing-fast parsing with comprehensive error reporting.

## Features

- **Zero-Copy Parsing**: All AST nodes reference the original source—no string allocations
- **Thread-Safe AST**: When using `Send + Sync` source types, the entire AST is `Send + Sync + 'static`
- **Dual Dialects**: Complete parsers for both standard GraphQL and GraphQLX
- **Generic Over Source Types**: Works with `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::HipStr`, and custom types
- **Comprehensive Error Reporting**: Detailed error messages with source location information
- **Parser Combinators**: Composable parsers built with the `chumsky` for parsing and `logos` for lexing.
- **`no_std` Compatible**: Works in embedded environments with optional `alloc` support

## Quick Start

### Parsing GraphQL

```rust
use smear_parser::graphql::{self, ParseStr};

// Parse a GraphQL schema
let schema = r#"
  type User {
    id: ID!
    name: String!
    email: String
  }
"#;

let ast = graphql::ast::TypeSystemDocument::parse_str(schema).unwrap();

// Parse a GraphQL query
let query = r#"
  query GetUser($id: ID!) {
    user(id: $id) {
      id
      name
      email
    }
  }
"#;

let ast = graphql::ast::ExecutableDocument::parse_str(query).unwrap();
```

### Parsing GraphQLx

GraphQLX extends GraphQL with generics, imports, type paths, and more:

```rust
use smear_parser::graphqlx::{self, ParseStr};

// Parse GraphQLX with generics and imports
let schema = r#"
  import { Node } from "./common.graphqlx"

  type Page<T> {
    items: [T!]!
    total: Int!
  }

  type Query {
    users: Page<User>!
  }
"#;

let ast = graphqlx::ast::TypeSystemDocument::parse_str(schema).unwrap();
```

## Supported Dialects

### GraphQL

The `graphql` module provides a complete parser for the GraphQL specification:

- **Type System**: Scalars, objects, interfaces, unions, enums, input objects, directives
- **Executable Definitions**: Queries, mutations, subscriptions, fragments
- **Full Spec Compliance**: Implements the complete GraphQL specification

### GraphQLX

The `graphqlx` module extends GraphQL with powerful features for schema composition:

- **Type Generics**: Parameterized types with type parameters (e.g., `Page<T>`)
- **Where Clauses**: Type constraints on generic parameters
- **Type Paths**: Namespaced type references (e.g., `user::User`, `::Global`)
- **Import System**: Module imports for schema composition
- **Extended Collections**: Map types (`<K => V>`) and set types (`<T>`)

## Parser Traits

The parser provides convenient traits for different source types:

```rust,ignore
use smear_parser::graphql::{ParseStr, ParseBytes, ParseBytesSlice};

// For &str sources
let ast = graphql::ast::TypeSystemDocument::parse_str(schema_str)?;

// For &[u8] sources
let ast = graphql::ast::TypeSystemDocument::parse_bytes_slice(schema_bytes)?;

// For owned Bytes
let ast = graphql::ast::TypeSystemDocument::parse_bytes(schema_bytes)?;
```

## AST Structure

The parser produces strongly-typed AST nodes:

```rust
use smear_parser::graphql::ast;

// Type system AST
type TypeSystemDocument = ast::TypeSystemDocument<&str>;
type ObjectTypeDefinition = ast::ObjectTypeDefinition<&str>;
type FieldDefinition = ast::FieldDefinition<&str>;

// Executable AST
type ExecutableDocument = ast::ExecutableDocument<&str>;
type OperationDefinition = ast::OperationDefinition<&str>;
type SelectionSet = ast::SelectionSet<&str>;
```

All AST types are generic over the source type `S`, allowing you to choose the source representation that fits your use case.

## Thread-Safe Parsing

Use thread-safe source types for concurrent processing:

```rust,ignore
use smear_parser::graphql::{self, ParseBytes};
use bytes::Bytes;

// Parse with Bytes for Send + Sync AST
let source = Bytes::from("type User { id: ID! }");
let ast = graphql::ast::TypeSystemDocument::parse_bytes(source).unwrap();

// AST is now Send + Sync + 'static
// Can be shared across threads or sent to other tasks
std::thread::spawn(move || {
    // Use ast here
});
```

## Error Handling

The parser provides detailed error information:

```rust
use smear_parser::graphql::{self, ParseStr};

let invalid = "type User { id: }"; // Missing type

match graphql::ast::TypeSystemDocument::parse_str(invalid) {
    Ok(ast) => println!("Parsed successfully"),
    Err(errors) => {
        for error in errors {
            println!("Error at {}: {}", error.span(), error);
        }
    }
}
```

## Feature Flags

| Feature | Description | Default |
|---------|-------------|---------|
| `std` | Standard library support | ✓ |
| `alloc` | Allocation support for `no_std` | |
| `graphql` | Standard GraphQL parser | ✓ |
| `graphqlx` | Extended GraphQL parser | ✓ |
| `unstable` | Unstable features (required for GraphQLX) | |
| `smallvec` | Use `smallvec` for small collections | |
| `bytes` | Support `bytes::Bytes` source type | |
| `bstr` | Support `bstr::BStr` source type | |
| `hipstr` | Support `hipstr::{HipStr, HipByt}` source type | |

## Architecture

This crate builds on the smear ecosystem:

```
┌─────────────┐
│ smear-lexer │  Zero-copy tokenization
└──────┬──────┘
       │
┌──────▼────────┐
│smear-scaffold │  Generic AST scaffolding
└──────┬────────┘
       │
┌──────▼────────┐
│ smear-parser  │  Parser combinators (this crate)
└───────────────┘
```

### Lexer Layer

[`smear-lexer`](https://crates.io/crates/smear-lexer) provides:
- Zero-copy tokenization
- Dual token streams (syntactic and lossless)
- Generic over source types

### Scaffold Layer

[`smear-scaffold`](https://crates.io/crates/smear-scaffold) provides:
- Generic, reusable AST node definitions
- Type-safe AST construction
- Shared between GraphQL and GraphQLX

### Parser Layer (This Crate)

Combines lexer and scaffold to provide:
- Complete parser implementations
- Parser combinator framework
- Error recovery and reporting

## Use Cases

**Ideal for:**

- GraphQL servers needing fast query parsing
- Schema analysis and validation tools
- GraphQL IDEs, linters, and formatters
- Building custom GraphQL-like DSLs
- Research projects exploring type systems

## Performance

The parser is designed for maximum performance:

- **Zero Allocations**: AST nodes hold slices into the original source
- **Fast Tokenization**: Efficient lexer based on `logos`
- **Parser Combinators**: Composable parsers with minimal overhead
- **Parallel Processing**: Thread-safe AST enables concurrent parsing

## Integration with smear

This crate is part of the smear ecosystem:

- [`smear`](https://crates.io/crates/smear): Top-level re-exports and documentation
- [`smear-lexer`](https://crates.io/crates/smear-lexer): Zero-copy tokenization
- [`smear-scaffold`](https://crates.io/crates/smear-scaffold): Generic AST scaffolding
- **`smear-parser`**: Parser combinators and AST construction (this crate)

For most use cases, you should use the top-level [`smear`](https://crates.io/crates/smear) crate, which re-exports all necessary types and provides comprehensive documentation.

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
