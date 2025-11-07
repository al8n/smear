# Smear Parser - Design Session Notes

## Project Vision

**Goal**: Build a generic GraphQL-dialect parser library that supports various GraphQL-like DSLs with slightly different schemas and syntax.

**Motivation**: Most projects are based on GraphQL but have dialect-specific variations:
- **Dgraph**: Custom directives (`@dgraph`, `@search`), reverse edges, different type system
- **The Graph Protocol**: `@entity`, `@derivedFrom`, custom scalars (Bytes, BigInt, BigDecimal)
- **Apollo Federation**: `@key`, `@external`, `@requires`, `@provides`
- **Hasura**: Permissions directives, relationship syntax extensions

**Market Gap**: Existing GraphQL parsers are monolithic and hardcoded for standard GraphQL. A flexible, dialect-agnostic parser library would fill a real need.

## Current Architecture

### Generic Scaffold Pattern

The core design uses generic type parameters to allow customization:

```rust
pub struct ScalarTypeDefinition<Name, Directives> {
  span: Span,
  name: Name,
  directives: Option<Directives>,
}
```

This enables:
- Type-safe dialect-specific ASTs
- Reusable parsing structure
- Strong compile-time guarantees

### Separation of Concerns

- **`ast_scaffold/`**: Generic AST structure (language-agnostic)
- **`graphql/`**: Standard GraphQL implementation
- **`graphqlx/`**: Extended GraphQL variant
- **`value/`**: Generic value types (variables, literals)

### Error Handling Strategy

Uses type-safe error construction with `IncompleteSyntax<T>`:
- Tracks which component of a syntax element is missing
- Type-level encoding prevents wrong error construction
- Works with `From` trait for idiomatic error propagation

## Recent Refactor (Completed)

### What Changed

Replaced custom error traits with idiomatic Rust `From<IncompleteSyntax<*Syntax>>` bounds.

**Removed 6 extension error traits:**
- `UnexpectedEndOfObjectExtensionError`
- `UnexpectedEndOfInterfaceExtensionError`
- `UnexpectedEndOfUnionExtensionError`
- `UnexpectedEndOfEnumExtensionError`
- `UnexpectedEndOfInputObjectExtensionError`
- `UnexpectedEndOfSchemaExtensionError`

**Simplified error handling:**
- Renamed `ParseVariableValueError` → `MissingDollarTokenError`
- Now only handles special case: missing `$` in variable (e.g., `foo` vs `$foo`)
- Incomplete syntax errors use auto-derived `From` implementations

**Key insight**: The `Error` enum derives `From`, which automatically generates all needed `From<IncompleteSyntax<*Syntax>>` implementations. This eliminated hundreds of lines of boilerplate.

### Benefits

- More idiomatic Rust code
- Reduced trait complexity
- Better maintainability
- Leverages standard library patterns

### Files Modified

- `src/scaffold_error.rs` - Removed 6 traits
- `src/error.rs` - Simplified to MissingDollarTokenError
- `src/graphql/ast/error.rs` - Updated implementations
- `src/graphqlx/ast/error.rs` - Updated implementations
- `src/value/variable.rs` - Uses new error bounds
- `src/ast_scaffold/definitions/*.rs` - Removed trait bounds (7 files)

## Professional Assessment Summary

### Strengths

1. **Sophisticated type-level design** - Excellent use of Rust's type system
2. **Correctness focus** - Type-safe error handling, separation of concerns
3. **Comprehensive documentation** - Good inline docs with examples
4. **Well-suited for goal** - Generic scaffold is perfect for dialect customization

### Current Limitations

1. **Missing ergonomic API layer** - Only exposes generic internals
2. **High barrier to entry** - Complex generic bounds everywhere
3. **No concrete dialect examples** - Leaves users to figure out parameterization
4. **45 unused type aliases** - Suggests uncertainty about abstractions

### Verdict

- **Technical Architecture**: 9/10 - Excellent for stated goal
- **Current Usability**: 6/10 - Missing user-facing ergonomics
- **Potential**: 10/10 - Could be THE GraphQL dialect parser library

**Key insight**: Architecture is sound, but needs an ergonomic wrapper layer for 80% use cases.

## Planned Improvements

### 1. Three-Layer API Design

```rust
// Layer 1: Power Users (current generic API)
impl<'a, Name, Directives, I, T, Error> Parseable<'a, I, T, Error>
  for ScalarTypeDefinition<Name, Directives>
where /* complex bounds */ { }

// Layer 2: Common Case (ADD THIS)
pub mod prelude {
  /// Standard GraphQL with concrete types
  pub type StandardScalar = ScalarTypeDefinition<String, StandardDirectives>;
  pub type StandardDocument = Document<StandardDefinition>;

  /// Parse standard GraphQL schema
  pub fn parse_standard_graphql(input: &str) -> Result<Document, Error> {
    // Pre-configured parser with standard types
  }
}

// Layer 3: Dialect Modules (ADD THIS)
pub mod dialects {
  pub mod dgraph {
    pub type DgraphScalar = ScalarTypeDefinition<DgraphName, DgraphDirectives>;
    pub type DgraphDocument = Document<DgraphDefinition>;

    // Pre-configured Dgraph parser
    pub fn parse_dgraph_schema(input: &str) -> Result<DgraphDocument, Error> { }
  }

  pub mod thegraph {
    // Similar structure for The Graph Protocol
  }

  pub mod federation {
    // Apollo Federation directives
  }
}
```

### 2. Type Alias Strategy

Hide generic complexity behind well-named aliases in each dialect:

```rust
// In dgraph module:
pub type Scalar = ScalarTypeDefinition<Name, Directives>;
pub type Object = ObjectTypeDefinition<Name, Implements, Directives, Fields>;
pub type Enum = EnumTypeDefinition<Name, Directives, Values>;

// Users write clean code:
use smear::dialects::dgraph::*;
let scalar: Scalar = parse_scalar(input)?;

// Instead of:
use smear::ast_scaffold::*;
let scalar: ScalarTypeDefinition<DgraphName, DgraphDirectives> = ...;
```

### 3. Builder/Configuration API

For simple customizations without full generic parameterization:

```rust
let parser = GraphQLParser::builder()
  .extend_scalars(&["Bytes", "BigInt", "BigDecimal", "Address"])
  .add_directive("entity", DirectiveLocation::Object)
  .add_directive("derivedFrom", DirectiveLocation::Field)
  .build();

let ast = parser.parse(input)?;
```

### 4. Documentation Tracks

Create three separate documentation paths:

**Track A: Quick Start (80% of users)**
- Using standard GraphQL parser
- Parsing and traversing AST
- Basic error handling
- Example: "Parse a GraphQL schema in 5 lines"

**Track B: Dialect Customization (15% of users)**
- "Building Your Own GraphQL Dialect"
- Step-by-step guide:
  1. Adding custom directives
  2. Custom scalar types
  3. Syntax extensions
  4. Complete dialect example
- Real-world case study: Implementing Dgraph

**Track C: Architecture Deep-Dive (5% of users)**
- Generic scaffold pattern explained
- Type-level design decisions
- Contributing guidelines
- Adding new AST node types

### 5. Concrete Dialect Examples

Ship with 3-4 production-ready dialects as templates:

```
smear/
  src/
    lib.rs
    scaffold/           # Generic core (mostly private)
    graphql/            # Standard GraphQL (concrete types)
    dialects/
      dgraph/           # Dgraph dialect
      thegraph/         # The Graph Protocol
      federation/       # Apollo Federation
    examples/
      custom_dialect/   # Tutorial: build from scratch
      ast_traversal/    # Working with parsed AST
      error_recovery/   # Handling parse errors
```

### 6. AstNode Trait (Proposed)

Add trait in logosky to bridge AST and syntax types:

```rust
pub trait AstNode<Lang> {
  type Syntax: Syntax<Lang = Lang>;
}

// Enables generic error handling:
impl<'a, T, I, Token, Error> Parseable<'a, I, Token, Error> for T
where
  T: AstNode<Lang>,
  Error: From<IncompleteSyntax<T::Syntax>>,
{
  // Generic parser implementation using T::Syntax automatically
}
```

**Benefits:**
- Type-safe AST-syntax relationship
- Better discoverability
- Reduces boilerplate in error handling
- Enables language-polymorphic code

## Success Factors

What makes or breaks this project:

### Critical for Success

1. **Ergonomic 80% case** - Standard GraphQL should be trivial
2. **Clear customization path** - Show how to add one directive step-by-step
3. **Real dialect examples** - Ship with 2-3 production dialects
4. **Good error messages** - Leverage IncompleteSyntax for helpful errors
5. **Reasonable compile times** - Keep example builds under 5 seconds

### Failure Modes to Avoid

1. **Only exposing generic APIs** - Forces complexity on all users
2. **Poor documentation** - Smart design but no one understands it
3. **No examples** - Users can't figure out parameterization
4. **Slow compilation** - Makes library impractical despite flexibility
5. **Confusing error messages** - Deep trait bound errors scare users away

## Next Steps

### Immediate Priorities

1. **Create `src/graphql/prelude.rs`** - Concrete standard GraphQL types
   - Standard document type with concrete type parameters
   - Simple parse function
   - Re-export common types

2. **Build example dialect** - `examples/dgraph_parser.rs`
   - Shows customizing scaffold for Dgraph
   - Custom directives (`@dgraph`, `@search`)
   - Working end-to-end example

3. **Write `DESIGN.md`** - Explain architecture
   - Why generic scaffold?
   - How to use it
   - Design philosophy

4. **Benchmark compilation** - `cargo build --example dgraph_parser`
   - Measure compile time cost
   - Profile if needed
   - Ensure it's practical

5. **Design simple API** - Sketch out builder pattern
   - How should 90% of users interact?
   - What's the minimal API surface?

### Future Work

- Implement AstNode trait in logosky
- Add more dialect examples (thegraph, federation)
- Write comprehensive tutorial documentation
- Create video walkthrough of building a custom dialect
- Set up benchmarks for parse performance
- Error message improvement pass

## Technical Debt / Cleanup

- **45 unused type aliases** in `graphqlx/error.rs` - Remove or document purpose
- **Compilation warnings** - All warnings are for unused type aliases (low priority)
- **Test coverage** - Some parser implementations lack tests
- **README doctest failure** - Fix `Errors::span()` example

## Context for Next Session

When resuming work:

1. **Start with prelude module** - Most impactful for users
2. **Or start with example** - Dgraph dialect shows practical use
3. **Or start with AstNode trait** - Foundational improvement

**Key files to reference:**
- `src/ast_scaffold/definitions/scalar_definition.rs` - Good example of scaffold pattern
- `src/value/variable.rs` - Shows new error handling approach
- `src/graphql/ast/error.rs` - Error trait implementations

**Recent changes:**
- All extension error traits removed
- New bound pattern: `Error: From<IncompleteSyntax<*Syntax>>`
- MissingDollarTokenError for special cases only

## Questions to Explore

1. Should scaffold types be public or internal implementation detail?
2. How much compile-time overhead is acceptable?
3. Should we support runtime dialect configuration or only compile-time?
4. How to handle dialect compatibility/versioning?
5. What's the migration story for users moving between dialects?

---

**Session Date**: 2025-11-07
**Status**: Architecture solid, needs ergonomic layer
**Next Focus**: User-facing API design and examples
