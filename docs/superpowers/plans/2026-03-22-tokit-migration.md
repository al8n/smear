# Migrate smear from logosky+chumsky to tokit

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Replace the logosky (0.1) + chumsky (0.11) parser combinator stack with the tokit library throughout the entire smear workspace (lexer, scaffold, parser, facade crates).

**Architecture:** The migration follows a bottom-up approach: workspace deps first, then lexer (token types + Lexer trait), then scaffold (utility traits + Parseable→ParseInput), then parser (all combinator usage), then facade. Each layer must compile before the next begins. The core paradigm shift is from chumsky's `impl Parser` return-type combinators to tokit's `ParseInput`/`TryParseInput` trait-based approach with `InputRef`.

**Tech Stack:** Rust, tokit (path dep), logos (via tokit re-export), derive_more

---

## Trait/Type Mapping Reference

This table maps logosky/chumsky concepts to their tokit equivalents. Every file in the migration uses these mappings.

| logosky/chumsky | tokit | Notes |
|---|---|---|
| `logosky::utils::Span` | `tokit::SimpleSpan` | Both are `Range<usize>` wrappers |
| `logosky::utils::AsSpan` | `tokit::span::AsSpan` | Same trait shape |
| `logosky::utils::IntoSpan` | `tokit::span::IntoSpan` | Same trait shape |
| `logosky::utils::IntoComponents` | `tokit::utils::IntoComponents` | Same trait shape |
| `logosky::Token<'a>` | `tokit::Token<'a>` | Different associated types |
| `logosky::Parseable<'a, I, T, Error>` | `tokit::ParseInput<'inp, L, O, Ctx, Lang>` | Fundamentally different - see below |
| `logosky::TokenStream<'a, T>` | `tokit::lexer::LogosLexer<'inp, T>` | Lexer adapter |
| `chumsky::Parser` trait | `tokit::ParseInput` / `tokit::TryParseInput` | |
| `chumsky::extra::Err<E>` | `tokit::Emitter` system | |
| `chumsky::extra::ParserExtra` | `tokit::ParseContext` | |
| `chumsky::container::Container<T>` | `tokit::Container<T>` | Similar interface |
| `Logos` derive | `Logos` derive (via `tokit::logos`) | Same logos crate |
| `any().try_map()` | `InputRef::next()` + match | Direct token access |
| `custom(\|inp\| ...)` | `fn(InputRef) -> Result<O, E>` | Free function or closure |
| `recursive(\|r\| ...)` | Manual recursion via `&mut dyn ParseInput` | No built-in recursive combinator |
| `.then()` | `.then()` on ParseInput | Similar |
| `.then_ignore()` | `.then_ignore()` on ParseInput | Similar |
| `.or()` | `.peek_then_choice()` | Deterministic dispatch |
| `.repeated().collect()` | `.repeated()` / `.repeated_while()` | Explicit condition |
| `.map_with(\|v, exa\| exa.span())` | `InputRef::span_since(cursor)` | Span from cursor |
| `logosky::utils::human_display::DisplayHuman` | `tokit::utils::human_display::DisplayHuman` | Same |
| `logosky::utils::sdl_display::DisplayCompact` | `tokit::utils::sdl_display::DisplayCompact` | Same |
| `logosky::utils::sdl_display::DisplayPretty` | `tokit::utils::sdl_display::DisplayPretty` | Same |
| `logosky::utils::syntax_tree_display::DisplaySyntaxTree` | `tokit::utils::syntax_tree_display::DisplaySyntaxTree` | Same |

### Parseable → ParseInput Paradigm Shift

**Before (logosky/chumsky):**
```rust
impl Parseable<'a, I, T, Error> for MyType {
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where E: ParserExtra<'a, I, Error = Error>
  {
    name_parser.then(value_parser).map_with(|(n, v), exa| Self::new(exa.span(), n, v))
  }
}
// Usage: MyType::parser::<Extra>().parse(token_stream)
```

**After (tokit):**
```rust
impl<'inp, L, Ctx, Lang> ParseInput<'inp, L, Self, Ctx, Lang> for MyTypeParser
where
  L: Lexer<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
{
  fn parse_input(&mut self, input: &mut InputRef<'inp, '_, L, Ctx, Lang>) -> Result<Self, Error> {
    let cursor = input.cursor();
    let name = name_parser.parse_input(input)?;
    let value = value_parser.parse_input(input)?;
    Ok(MyType::new(input.span_since(&cursor), name, value))
  }
}
// Usage: Parser::new().apply(my_type_parser).parse_str(source)
```

Key differences:
1. **No `impl Parser` return** - parsers are structs implementing `ParseInput`, or free functions
2. **Explicit span tracking** - use `input.cursor()` + `input.span_since()` instead of `map_with`
3. **Mutable InputRef** - parsers take `&mut InputRef` instead of consuming token streams
4. **Context-based errors** - errors flow through `Ctx::Emitter` instead of `ParserExtra`
5. **No `recursive()`** - use `Box<dyn ParseInput>` or function pointers for recursion

---

## File Structure Overview

### Files Modified (by crate)

**Workspace root:**
- Modify: `Cargo.toml` — replace logosky/chumsky deps with tokit

**smear-lexer (46 .rs files, ~20 need changes):**
- Modify: `Cargo.toml` — swap logosky dep for tokit
- Modify: `src/lib.rs` — re-export tokit instead of logosky
- Modify: `src/graphql/syntactic/token.rs` — Token trait impl
- Modify: `src/graphql/lossless/token.rs` — Token trait impl
- Modify: `src/graphqlx/ast/token.rs` — Token trait impl (if exists)
- Modify: `src/graphqlx/lossless/token.rs` — Token trait impl
- Modify: `src/keywords.rs` — macro uses tokit span/display types
- Modify: `src/punctuator.rs` — macro uses tokit span/display types
- Modify: `src/handlers.rs` — logosky lexer utilities → tokit
- Modify: `src/string_lexer.rs` + subdirectory — Lexable trait
- Modify: `src/graphql/syntactic.rs` — type aliases
- Modify: `src/graphql/lossless.rs` — type aliases
- Modify: `src/graphqlx/ast.rs` — type aliases
- Modify: `src/error.rs` — error types

**smear-scaffold (52 files use logosky):**
- Modify: `Cargo.toml` — swap logosky dep for tokit
- Modify: All 52 files — replace `logosky::` imports with `tokit::` equivalents
- Major pattern: `Parseable` impls → `ParseInput` impls in all `parser_with` and `Parseable` blocks

**smear-parser (60+ files use chumsky/logosky):**
- Modify: `Cargo.toml` — swap deps
- Modify: `src/graphql/ast.rs` — type aliases, ParseStr trait
- Modify: `src/graphql/error.rs` — error types
- Modify: All parser files in `src/graphql/ast/` — combinator migration
- Modify: All parser files in `src/graphqlx/ast/` — combinator migration
- Modify: `src/value/` — shared value parsers
- Modify: CST files in `src/graphql/impls/lossless/` — lossless parser

**smear (facade):**
- Modify: `Cargo.toml` — swap deps
- Modify: `src/lib.rs` — re-exports

---

## Task Breakdown

### Task 1: Update Workspace Dependencies

**Files:**
- Modify: `Cargo.toml` (workspace root)
- Modify: `smear/Cargo.toml`
- Modify: `smear-lexer/Cargo.toml`
- Modify: `smear-scaffold/Cargo.toml`
- Modify: `smear-parser/Cargo.toml`

- [ ] **Step 1: Update workspace root Cargo.toml**

Replace:
```toml
chumsky = { version = "0.11", default-features = false }
logosky = { version = "0.1", default-features = false }
```
With:
```toml
tokit = { path = "../tokit/tokit", default-features = false }
```

Ensure `logos` is available via tokit's `logos` feature. Update feature forwarding.

- [ ] **Step 2: Update each crate's Cargo.toml**

In each sub-crate, replace `logosky.workspace = true` with `tokit.workspace = true`. Remove any direct `chumsky` references. Update feature gates (e.g., `logos/bytes` becomes managed through tokit features).

- [ ] **Step 3: Verify workspace compiles (expect errors)**

Run: `cargo check 2>&1 | head -50`
Expected: Compilation errors from missing logosky/chumsky imports (this is fine — confirms deps are correctly swapped).

- [ ] **Step 4: Commit**

```bash
git add Cargo.toml smear*/Cargo.toml
git commit -m "build: replace logosky+chumsky deps with tokit"
```

---

### Task 2: Migrate smear-lexer — Core Types and Re-exports

**Files:**
- Modify: `smear-lexer/src/lib.rs`
- Modify: `smear-lexer/src/error.rs`

- [ ] **Step 1: Update lib.rs re-exports**

Replace `pub use logosky;` with `pub use tokit;`. Update the `__private` module to re-export tokit instead of logosky.

- [ ] **Step 2: Update error.rs**

Replace `use logosky::utils::Span;` with `use tokit::SimpleSpan as Span;` (or directly use `SimpleSpan`).

- [ ] **Step 3: Commit**

```bash
git add smear-lexer/src/lib.rs smear-lexer/src/error.rs
git commit -m "refactor(lexer): update core re-exports from logosky to tokit"
```

---

### Task 3: Migrate smear-lexer — Keyword and Punctuator Macros

**Files:**
- Modify: `smear-lexer/src/keywords.rs`
- Modify: `smear-lexer/src/punctuator.rs`

- [ ] **Step 1: Update keywords.rs macro**

Replace all `logosky::utils::Span` with `tokit::SimpleSpan`. Replace `$crate::__private::logosky::utils::` paths with `$crate::__private::tokit::`. Update trait paths: `AsSpan`, `IntoSpan`, `IntoComponents`, `DisplayHuman`, `DisplayCompact`, `DisplayPretty`, `DisplaySyntaxTree`.

- [ ] **Step 2: Update punctuator.rs macro**

Same substitutions as keywords.rs. The `punctuator!` macro generates structs with `span: Span` fields and trait impls — update all `logosky::` references to `tokit::`.

- [ ] **Step 3: Commit**

```bash
git add smear-lexer/src/keywords.rs smear-lexer/src/punctuator.rs
git commit -m "refactor(lexer): migrate keyword/punctuator macros to tokit"
```

---

### Task 4: Migrate smear-lexer — Token Definitions

**Files:**
- Modify: `smear-lexer/src/graphql/syntactic/token.rs`
- Modify: `smear-lexer/src/graphql/lossless/token.rs`
- Modify: `smear-lexer/src/graphqlx/ast/token.rs` (if feature-gated)
- Modify: `smear-lexer/src/graphqlx/lossless/token.rs` (if feature-gated)

- [ ] **Step 1: Update GraphQL SyntacticToken**

The `token!` macro generates token enums. Update:
- `#[logos(crate = logosky::logos)]` → `#[logos(crate = tokit::logos)]`
- `logosky::Token<'b>` impl → `tokit::Token<'b>` impl (note: tokit's Token has `type Kind` and `type Error` associated types, plus `fn kind()` and `fn is_trivia()`)
- `logosky::utils::recursion_tracker::*` → find tokit equivalent or keep as local utility
- `Logos` derive stays (same logos crate, different re-export path)

Key difference: tokit's `Token` trait has `type Error` (not just `Kind`/`Char`/`Logos`). Need to add `type Error` associated type.

- [ ] **Step 2: Update GraphQL LosslessToken**

Same pattern as SyntacticToken. Also update `Tracker` import paths.

- [ ] **Step 3: Update GraphQLx tokens (behind feature gate)**

Same pattern for the GraphQLx token variants.

- [ ] **Step 4: Update type aliases in graphql/syntactic.rs and graphql/lossless.rs**

Replace:
```rust
pub type Lexer<'a, S> = logosky::TokenStream<'a, SyntacticToken<S>>;
```
With:
```rust
pub type Lexer<'a, S> = tokit::lexer::LogosLexer<'a, SyntacticToken<S>>;
```

- [ ] **Step 5: Commit**

```bash
git add smear-lexer/src/graphql/ smear-lexer/src/graphqlx/
git commit -m "refactor(lexer): migrate token definitions to tokit Token trait"
```

---

### Task 5: Migrate smear-lexer — Handlers and String Lexers

**Files:**
- Modify: `smear-lexer/src/handlers.rs`
- Modify: `smear-lexer/src/string_lexer.rs`
- Modify: `smear-lexer/src/string_lexer/inline/*.rs`
- Modify: `smear-lexer/src/string_lexer/block/*.rs`

- [ ] **Step 1: Update handlers.rs**

Replace all `logosky::` imports with `tokit::` equivalents:
- `logosky::logos::Lexer` → `tokit::logos::Lexer`
- `logosky::utils::*` → `tokit::utils::*` / `tokit::SimpleSpan`
- `logosky::Logos` → `tokit::logos::Logos`

The handler functions (increase_recursion_depth, tt_hook, etc.) should keep their signatures but use tokit types. Check if tokit has RecursionLimiter/Tracker equivalents or if smear needs to keep these locally.

- [ ] **Step 2: Update string_lexer module**

Replace `logosky::logos::Lexer` and display trait imports with tokit equivalents. The `Lexable` trait from logosky needs to either be ported to use tokit's API or kept as a local trait.

- [ ] **Step 3: Verify smear-lexer compiles**

Run: `cargo check -p smear-lexer 2>&1 | head -50`
Fix any remaining compilation errors.

- [ ] **Step 4: Commit**

```bash
git add smear-lexer/
git commit -m "refactor(lexer): complete migration of handlers and string lexers to tokit"
```

---

### Task 6: Migrate smear-scaffold — Utility Trait Imports (Batch 1: definitions/)

**Files:**
- Modify: All files in `smear-scaffold/src/ast/definitions/` (30+ files)
- Modify: `smear-scaffold/src/ast/and.rs`
- Modify: `smear-scaffold/src/error.rs`

- [ ] **Step 1: Replace imports across all definition files**

In every file, replace the common import block:
```rust
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{self, IterParser, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span},
};
```
With the tokit equivalent:
```rust
use tokit::{
  Lexer, ParseInput, Source, Token, SimpleSpan as Span,
  span::{AsSpan, IntoSpan},
  utils::IntoComponents,
  // ParseContext, Emitter, InputRef — as needed per file
};
```

Note: Files that only use `AsSpan`/`IntoSpan`/`IntoComponents`/`Span` (no parser code) just need the import swap. Files with `Parseable` impls need the full paradigm change (Task 7).

- [ ] **Step 2: Update Span type usage**

All `Span` usages were `logosky::utils::Span`. Replace with `tokit::SimpleSpan`. Consider adding a type alias `type Span = tokit::SimpleSpan;` in the scaffold crate root for convenience.

- [ ] **Step 3: Commit**

```bash
git add smear-scaffold/
git commit -m "refactor(scaffold): replace logosky utility imports with tokit equivalents"
```

---

### Task 7: Migrate smear-scaffold — Parseable→ParseInput (Batch 2: lang/ and definitions/)

This is the largest task. Every `Parseable` impl and `parser_with` method must change.

**Files:**
- Modify: All 52 files in smear-scaffold that have `Parseable` impls

- [ ] **Step 1: Define the new parser pattern for scaffold**

The scaffold currently provides `parser_with()` methods that return `impl Parser`. These need to become functions that take `&mut InputRef` and return `Result<Self, Error>`.

New pattern for `parser_with`:
```rust
pub fn parse_with<'inp, L, Ctx, Lang, N, V>(
  input: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  name_parser: &mut N,
  value_parser: &mut V,
) -> Result<Self, <Ctx::Emitter as Emitter<'inp, L, Lang>>::Error>
where
  L: Lexer<'inp>,
  Ctx: ParseContext<'inp, L, Lang>,
  N: ParseInput<'inp, L, Name, Ctx, Lang>,
  V: ParseInput<'inp, L, Value, Ctx, Lang>,
{
  let cursor = input.cursor();
  let name = name_parser.parse_input(input)?;
  // ... parse colon ...
  let value = value_parser.parse_input(input)?;
  Ok(Self { span: input.span_since(&cursor), name, value })
}
```

- [ ] **Step 2: Migrate simple scaffold types (arguments.rs, directives.rs, etc.)**

Convert files one at a time. For each:
1. Replace the `Parseable` impl with a `parse_with` or `parse` method
2. Replace `chumsky::container::Container` with `tokit::Container`
3. Replace `.then()`, `.then_ignore()`, `.map_with()` chains with sequential `parse_input()` calls
4. Replace `.repeated().collect()` with tokit's `repeated_while()` or manual loop

- [ ] **Step 3: Migrate complex scaffold types (selection_set, field, fragment, input_value)**

These have recursive patterns. Convert `recursive()` calls to use `Box<dyn ParseInput>` or function pointers.

- [ ] **Step 4: Migrate type definition scaffolds (ty/, generic/)**

Convert all type-related parsers: named_type, list_type, set_type, map_type, angle_type, and generic type system scaffolds.

- [ ] **Step 5: Verify smear-scaffold compiles**

Run: `cargo check -p smear-scaffold 2>&1 | head -50`
Fix remaining issues.

- [ ] **Step 6: Commit**

```bash
git add smear-scaffold/
git commit -m "refactor(scaffold): migrate all Parseable impls to tokit ParseInput"
```

---

### Task 8: Migrate smear-parser — Error Types and Core Infrastructure

**Files:**
- Modify: `smear-parser/Cargo.toml`
- Modify: `smear-parser/src/graphql/error.rs`
- Modify: `smear-parser/src/graphql/ast.rs`
- Modify: `smear-parser/src/graphqlx/error.rs`
- Modify: `smear-parser/src/graphqlx/ast.rs`

- [ ] **Step 1: Update error types**

Replace:
```rust
pub type Extra<S, T, Char, Expectation, StateError> =
  logosky::chumsky::extra::Err<Errors<S, T, Char, Expectation, StateError>>;
```

The error system needs to integrate with tokit's `Emitter` trait. The `Errors` type should implement `From` for tokit's error types. Remove chumsky's `Error` and `LabelError` trait impls, add tokit `Emitter` compatibility.

- [ ] **Step 2: Update type aliases in graphql/ast.rs**

Replace:
```rust
pub type SyntacticTokenStream<'a, S> = logosky::TokenStream<'a, SyntacticToken<S>>;
```
With:
```rust
pub type SyntacticLexer<'a, S> = tokit::lexer::LogosLexer<'a, SyntacticToken<S>>;
```

Update the `ParseStr` trait to use tokit's `Parse` trait or a custom entry point.

- [ ] **Step 3: Same for graphqlx/ast.rs and graphqlx/error.rs**

Mirror the changes for GraphQLx types.

- [ ] **Step 4: Commit**

```bash
git add smear-parser/
git commit -m "refactor(parser): migrate error types and core infra to tokit"
```

---

### Task 9: Migrate smear-parser — GraphQL Simple Parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/name.rs`
- Modify: `smear-parser/src/graphql/ast/keyword.rs`
- Modify: `smear-parser/src/graphql/ast/punctuator.rs`
- Modify: `smear-parser/src/graphql/ast/location.rs`
- Modify: `smear-parser/src/graphql/ast/operation_type.rs`

- [ ] **Step 1: Migrate Name parser**

Replace:
```rust
any().try_map(|res: Lexed<'_, SyntacticToken<S>>, span| match res { ... })
```
With:
```rust
fn parse_input(&mut self, input: &mut InputRef<...>) -> Result<Name<S>, Error> {
  let cursor = input.cursor();
  match input.next() {
    Some(Ok(token)) => match token.kind() {
      TokenKind::Identifier => Ok(Name::new(input.span_since(&cursor), ...)),
      _ => Err(Error::unexpected_token(...)),
    },
    Some(Err(e)) => Err(Error::from_lexer_error(e, ...)),
    None => Err(Error::unexpected_eot(...)),
  }
}
```

- [ ] **Step 2: Migrate keyword, punctuator, location, operation_type parsers**

Same pattern: replace `any().try_map()` or `just(token)` with direct `InputRef` operations.

- [ ] **Step 3: Run cargo check on smear-parser**

- [ ] **Step 4: Commit**

```bash
git add smear-parser/src/graphql/ast/
git commit -m "refactor(parser): migrate GraphQL simple parsers to tokit"
```

---

### Task 10: Migrate smear-parser — GraphQL Value Parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/value.rs`
- Modify: `smear-parser/src/graphql/ast/value/boolean_value.rs`
- Modify: `smear-parser/src/graphql/ast/value/enum_value.rs`
- Modify: `smear-parser/src/graphql/ast/value/float.rs`
- Modify: `smear-parser/src/graphql/ast/value/int.rs`
- Modify: `smear-parser/src/graphql/ast/value/null_value.rs`
- Modify: `smear-parser/src/graphql/ast/value/string.rs`
- Modify: `smear-parser/src/value/variable.rs`

- [ ] **Step 1: Migrate simple value parsers (bool, null, int, float, string, enum)**

Each is a single-token parser. Convert from `any().try_map()` to direct `InputRef::next()` matching.

- [ ] **Step 2: Migrate composite InputValue parser**

This is the most complex parser — uses `recursive()` for nested values (lists, objects). Convert to:
- A struct implementing `ParseInput`
- Use `Box<dyn ParseInput>` for the recursive reference
- Match on token kind to dispatch to sub-parsers

- [ ] **Step 3: Commit**

```bash
git add smear-parser/src/graphql/ast/value/ smear-parser/src/value/
git commit -m "refactor(parser): migrate GraphQL value parsers to tokit"
```

---

### Task 11: Migrate smear-parser — GraphQL Type Parser

**Files:**
- Modify: `smear-parser/src/graphql/ast/ty.rs`

- [ ] **Step 1: Migrate Type parser**

The type parser handles recursive list/non-null types (`[Type]!`). Convert from chumsky's recursive combinator to manual recursion via `ParseInput`.

- [ ] **Step 2: Commit**

```bash
git add smear-parser/src/graphql/ast/ty.rs
git commit -m "refactor(parser): migrate GraphQL type parser to tokit"
```

---

### Task 12: Migrate smear-parser — GraphQL Fragment and Field Parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/fragment.rs`
- Modify: `smear-parser/src/graphql/ast/default/field.rs`

- [ ] **Step 1: Migrate fragment parser**

Convert FragmentSpread, InlineFragment, and FragmentDefinition parsers. These use `choice()` for alternatives — convert to `peek_then_choice()` or manual peek+dispatch.

- [ ] **Step 2: Migrate field parser**

This is the most complex with nested `recursive()` for fields→selection sets→fields. Convert to mutual recursion via `Box<dyn ParseInput>`.

- [ ] **Step 3: Commit**

```bash
git add smear-parser/src/graphql/ast/fragment.rs smear-parser/src/graphql/ast/default/
git commit -m "refactor(parser): migrate GraphQL fragment and field parsers to tokit"
```

---

### Task 13: Migrate smear-parser — GraphQL Type System Parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/type_system.rs`
- Modify: Related default/ files for type system definitions

- [ ] **Step 1: Migrate TypeDefinition and TypeExtension parsers**

These use `choice()` heavily to dispatch between scalar/object/interface/union/enum/input types. Convert to `peek_then_choice()` with token-based lookahead (each starts with a distinct keyword).

- [ ] **Step 2: Migrate SchemaDefinition parser**

- [ ] **Step 3: Commit**

```bash
git add smear-parser/src/graphql/ast/type_system.rs smear-parser/src/graphql/ast/default/
git commit -m "refactor(parser): migrate GraphQL type system parsers to tokit"
```

---

### Task 14: Migrate smear-parser — GraphQLx Parsers

**Files:**
- Modify: All files in `smear-parser/src/graphqlx/ast/`
- Modify: All files in `smear-parser/src/graphqlx/ast/default/`

- [ ] **Step 1: Migrate GraphQLx simple parsers (ident, keyword, punctuator, location, operation_type)**

Same patterns as GraphQL equivalents.

- [ ] **Step 2: Migrate GraphQLx value parsers**

- [ ] **Step 3: Migrate GraphQLx type parser (with generics, path, set, map)**

More complex than GraphQL due to angle brackets, path separators, and generic parameters.

- [ ] **Step 4: Migrate GraphQLx field, fragment, and type system parsers**

- [ ] **Step 5: Migrate GraphQLx import parser**

- [ ] **Step 6: Commit**

```bash
git add smear-parser/src/graphqlx/
git commit -m "refactor(parser): migrate all GraphQLx parsers to tokit"
```

---

### Task 15: Migrate smear-parser — Lossless/CST Parsers

**Files:**
- Modify: All files in `smear-parser/src/graphql/impls/lossless/ast/`

- [ ] **Step 1: Migrate lossless token parsers**

These parse using `LosslessToken` instead of `SyntacticToken`. Apply the same tokit patterns.

- [ ] **Step 2: Commit**

```bash
git add smear-parser/src/graphql/impls/
git commit -m "refactor(parser): migrate lossless/CST parsers to tokit"
```

---

### Task 16: Migrate smear Facade Crate

**Files:**
- Modify: `smear/src/lib.rs`
- Modify: `smear/src/hints.rs`

- [ ] **Step 1: Update re-exports and hints**

Replace any logosky references in the facade. Ensure public API remains compatible where possible.

- [ ] **Step 2: Commit**

```bash
git add smear/src/
git commit -m "refactor: update smear facade crate for tokit"
```

---

### Task 17: Fix Compilation and Run Tests

**Files:**
- Potentially any file across the workspace

- [ ] **Step 1: Run full workspace check**

Run: `cargo check --workspace --all-features 2>&1`
Fix all remaining compilation errors iteratively.

- [ ] **Step 2: Run existing tests**

Run: `cargo test --workspace 2>&1`
Fix failing tests. Most tests should pass with the same behavior — the public AST types haven't changed.

- [ ] **Step 3: Run benchmarks (sanity check)**

Run: `cargo bench --workspace 2>&1 | head -20`
Verify benchmarks still compile and produce reasonable results.

- [ ] **Step 4: Final commit**

```bash
git add -A
git commit -m "refactor: complete migration from logosky+chumsky to tokit"
```

---

### Task 18: Cleanup

- [ ] **Step 1: Remove logosky/chumsky from Cargo.lock**

Run: `cargo update` to clean up the lockfile.

- [ ] **Step 2: Search for any remaining logosky/chumsky references**

Run: `rg "logosky|chumsky" --type rust`
Fix any stragglers.

- [ ] **Step 3: Final commit**

```bash
git add -A
git commit -m "chore: remove all logosky/chumsky references"
```
