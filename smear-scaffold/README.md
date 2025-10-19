# smear-scaffold

Generic, reusable AST scaffolding for GraphQL and GraphQL-like DSLs.

## Overview

`smear-scaffold` is the foundational layer of the [smear](https://github.com/al8n/smear) parser library, providing generic, reusable Abstract Syntax Tree (AST) node definitions. This crate is designed to enable the creation of custom GraphQL-like Domain Specific Languages (DSLs) by offering a comprehensive set of parameterized AST components.

## Purpose

The scaffold layer serves as the blueprint for building both the standard GraphQL parser and GraphQLx (the extended GraphQL dialect), as well as any custom GraphQL-like languages. By abstracting common AST patterns into generic structures, it provides:

- **Reusability**: Share AST node definitions across multiple DSL implementations
- **Type Safety**: Leverage Rust's type system to ensure correctness at compile time
- **Flexibility**: Customize AST nodes through generic parameters for different source types and extensions
- **Zero-Copy**: All AST nodes hold references to the original source when possible

## Architecture

The scaffold is designed as a three-tier structure:

### 1. Core Language Components (`lang/`)

Fundamental building blocks that represent the core GraphQL language constructs:

- **Identifiers and Names**: Generic over source types (`&str`, `&[u8]`, etc.)
- **Basic Types**: List types, nullable types, named types
- **Values**: Input values (scalars, lists, objects, enums)
- **Directives**: Directive applications and definitions

### 2. Type System Definitions (`definitions/`)

Structures representing GraphQL schema definitions:

- **Type Definitions**: Scalars, objects, interfaces, unions, enums, input objects
- **Type Extensions**: Extending existing types with additional fields or directives
- **Schema Definitions**: Root operation type definitions
- **Directive Definitions**: Custom directive declarations

### 3. Executable Definitions

Components for GraphQL operations and fragments:

- **Operations**: Queries, mutations, subscriptions
- **Fragments**: Named fragments and inline fragments
- **Selections**: Field selections and selection sets
- **Variables**: Variable definitions and references

## Generic Type Parameters

Most scaffold types are generic over one or more parameters:

- **`S`**: Source type (e.g., `&str`, `&[u8]`, `bytes::Bytes`, `hipstr::HipStr`)
- **`Name`**: Identifier/name type
- **`Type`**: Type reference type
- **`Value`**: Input value type
- **`Directive`**: Directive type

This generality allows the same scaffold to support both standard GraphQL and extended variants like GraphQLx.

## GraphQLx Extensions

The scaffold includes optional support for GraphQLx features through generic parameters:

- **Type Generics**: Parameterized types with generic type parameters
- **Where Clauses**: Type constraints for generic parameters
- **Type Paths**: Namespaced and qualified type references
- **Import System**: Module imports for schema composition
- **Extended Collections**: Map and set types in addition to lists

## Features

### `std` (default)

Enables standard library support.

### `alloc`

Enables allocation support for `no_std` environments. This feature allows using heap-allocated types like `Vec` and `String` in embedded or WASM contexts without the full standard library.

## Usage

The scaffold is typically not used directly by end users. Instead, it's consumed by higher-level parser implementations.

For most use cases, you should use the [`smear-parser`](https://crates.io/crates/smear-parser) crate, which builds complete GraphQL and GraphQLx parsers on top of this scaffold.

## Integration with smear

This crate is part of the smear ecosystem:

- [`smear`](https://crates.io/crates/smear): Top-level re-exports and documentation
- [`smear-lexer`](https://crates.io/crates/smear-lexer): Zero-copy tokenization
- **`smear-scaffold`**: Generic AST scaffolding (this crate)
- [`smear-parser`](https://crates.io/crates/smear-parser): Parser combinators and AST construction

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
