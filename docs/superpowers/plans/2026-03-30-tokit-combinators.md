# Refactor Parsers to Use tokit Combinators with Generic Source Types

> **For agentic workers:** REQUIRED SUB-SKILL: Use superpowers:subagent-driven-development (recommended) or superpowers:executing-plans to implement this plan task-by-task. Steps use checkbox (`- [ ]`) syntax for tracking.

**Goal:** Rewrite smear-parser to use tokit's built-in parser combinators (peek_then_choice, repeated_while, etc.), support generic source types (not hardcoded `&str`), and export atomic parser combinators as public API.

**Architecture:** Replace the current hand-rolled recursive-descent parsers (free functions with manual `next_token`/`peek_kind` calls hardcoded to `&str`) with properly generic parser functions that use tokit combinators (`peek_then_choice` for dispatch, `repeated_while` for collections, `map_with` for spans). Export every parser function as `pub` so users can compose atomic parsers. Unify `parse_str_impl.rs` (str-only) with the generic parsers in the ast modules.

**Tech Stack:** Rust, tokit (ParseInput, TryParseInput, peek_then_choice, repeated_while, InputRef, FatalContext)

---

## Key Design Decisions

### 1. Parser Shape: Generic free functions

All parser functions follow this signature pattern:
```rust
pub fn parse_name<'inp, S, Ctx, Lang>(
  input: &mut InputRef<'inp, '_, SyntacticLexer<'inp, S>, Ctx, Lang>,
) -> Result<Name<S>, SyntacticTokenErrors<S>>
where
  S: Clone,
  SyntacticToken<S>: FromLogos<'inp>,
  SyntacticLexer<'inp, S>: Lexer<'inp, Token = SyntacticToken<S>, Span = SimpleSpan>,
  Ctx: ParseContext<'inp, SyntacticLexer<'inp, S>, Lang>,
  Ctx::Emitter: Emitter<'inp, SyntacticLexer<'inp, S>, Lang, Error = SyntacticTokenErrors<S>>,
  str: Equivalent<S>,
  Lang: ?Sized,
```

The `str: Equivalent<S>` bound is needed for keyword matching. These already exist in the atomic parsers (name.rs, keyword.rs, etc.) — the problem is `parse_str_impl.rs` duplicates everything hardcoded to `&str`.

### 2. Trait bounds macro

A macro to reduce where-clause repetition across all parsers:
```rust
macro_rules! syntactic_bounds {
  ($S:ident, $inp:lifetime, $Ctx:ident, $Lang:ident) => {
    $S: Clone,
    SyntacticToken<$S>: FromLogos<$inp>,
    SyntacticLexer<$inp, $S>: Lexer<$inp, Token = SyntacticToken<$S>, Span = SimpleSpan>,
    $Ctx: ParseContext<$inp, SyntacticLexer<$inp, $S>, $Lang>,
    <$Ctx as ParseContext<$inp, SyntacticLexer<$inp, $S>, $Lang>>::Emitter:
      Emitter<$inp, SyntacticLexer<$inp, $S>, $Lang, Error = SyntacticTokenErrors<$S>>,
    str: Equivalent<$S>,
    $Lang: ?Sized,
  };
}
```

### 3. Where to use tokit combinators

| Pattern | Current (manual) | New (tokit combinator) |
|---|---|---|
| Dispatch on token kind | `match peek_kind(input) { ... }` | `peek_then_choice::<_, U1>` with `Branch` |
| Parse list until closer | `while peek_kind != RBrace { ... }` | `repeated_while::<_, U1>` with token-check decision |
| Optional parse | `if peek_kind == X { Some(...) } else { None }` | `TryParseInput` returning `ParseAttempt::Decline` |
| Token sequence | `let a = p_a(input)?; let b = p_b(input)?;` | Keep sequential (`.then()` adds type complexity for no gain) |
| Pipe-separated items | `while peek_kind == Pipe { ... }` | `separated_while::<Pipe, _, U1>` or manual loop |

**Important**: `.then()` chains add significant type complexity in Rust. For sequences of different types (name, colon, value), keep using sequential `parse_input()` calls. Use tokit combinators where they provide real value: dispatch, repetition, and optional parsing.

### 4. ParseStr becomes generic

Delete `parse_str_impl.rs` entirely. Replace `ParseStr<'a>` (str-only) with generic implementation that delegates to the generic parser functions, e.g.:
```rust
impl<'a> ParseStr<'a> for Document<&'a str> {
  fn parse_str(input: &'a str) -> Result<Self, ...> {
    Parser::with_parser(parse_document).parse_str(input)
  }
}
```

---

## File Structure

### Files to Delete
- `smear-parser/src/graphql/ast/parse_str_impl.rs` — replaced by generic parsers

### Files to Modify (GraphQL)
- `smear-parser/src/graphql/ast.rs` — add bounds macro, update ParseStr, remove str-only helpers
- `smear-parser/src/graphql/ast/name.rs` — already generic, keep
- `smear-parser/src/graphql/ast/keyword.rs` — already generic, reduce boilerplate with macro
- `smear-parser/src/graphql/ast/punctuator.rs` — already generic, keep
- `smear-parser/src/graphql/ast/value.rs` — make fully generic (currently generic)
- `smear-parser/src/graphql/ast/value/*.rs` — already generic, keep
- `smear-parser/src/graphql/ast/ty.rs` — add generic `parse_type` function
- `smear-parser/src/graphql/ast/fragment.rs` — add generic fragment/inline-fragment parsers
- `smear-parser/src/graphql/ast/type_system.rs` — add generic type system parsers with `peek_then_choice`
- `smear-parser/src/graphql/ast/default/field.rs` — add generic field/selection/selection_set parsers
- `smear-parser/src/graphql/ast/default.rs` — add re-exports for all new parsers

### Files to Create
- `smear-parser/src/graphql/ast/combinators.rs` — shared combinator helpers (description, const_directives, arguments, etc.)

---

## Task Breakdown

### Task 1: Add bounds macro and shared combinator helpers

**Files:**
- Modify: `smear-parser/src/graphql/ast.rs`
- Create: `smear-parser/src/graphql/ast/combinators.rs`

- [ ] **Step 1: Add `syntactic_bounds!` macro to ast.rs**

Define a macro that expands to the common where clause for all graphql parser functions. This drastically reduces boilerplate.

- [ ] **Step 2: Create combinators.rs with generic shared parsers**

Move these from parse_str_impl.rs to generic functions:
- `pub fn parse_description` — optional string value (peek for InlineString/BlockString)
- `pub fn parse_const_argument` — Name ':' ConstValue
- `pub fn parse_const_arguments` — optional '(' ConstArgument+ ')'
- `pub fn parse_const_directive` — '@' Name Arguments?
- `pub fn parse_const_directives` — optional Directive+ (using `repeated_while` checking for `@`)
- `pub fn parse_argument` — Name ':' Value
- `pub fn parse_arguments` — optional '(' Argument+ ')'
- `pub fn parse_directive` — '@' Name Arguments?
- `pub fn parse_directives` — optional Directive+ (using `repeated_while`)
- `pub fn parse_default_value` — optional '=' ConstValue
- `pub fn parse_input_value_definition` — Description? Name ':' Type DefaultValue? Directives?
- `pub fn parse_arguments_definition` — optional '(' InputValueDefinition+ ')'

All must be generic over `S`, `Ctx`, `Lang`.

- [ ] **Step 3: Wire combinators.rs into the module tree**

Add `pub mod combinators;` to ast.rs and re-export.

- [ ] **Step 4: Verify compilation**

Run: `cargo check -p smear-parser --features graphql --no-default-features`

- [ ] **Step 5: Commit**

```bash
git commit -m "feat(parser): add shared generic combinator helpers"
```

---

### Task 2: Add generic type parser with peek_then_choice

**Files:**
- Modify: `smear-parser/src/graphql/ast/ty.rs`

- [ ] **Step 1: Add `parse_type` function**

Generic type parser using `peek_then_choice::<_, U1>` to dispatch between `[ListType]` and `NamedType`:
```rust
pub fn parse_type<'inp, S, Ctx, Lang>(
  input: &mut InputRef<...>,
) -> Result<Type<Name<S>>, SyntacticTokenErrors<S>>
{
  // peek_then_choice: LBracket → list type, Identifier → named type
}
```

The list type branch recursively calls `parse_type`.

- [ ] **Step 2: Verify and commit**

---

### Task 3: Add generic field, selection, and selection_set parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/default/field.rs`
- Modify: `smear-parser/src/graphql/ast/fragment.rs`

- [ ] **Step 1: Add `parse_field` function**

Generic field parser: Name (with optional alias via colon lookahead), Arguments?, Directives?, SelectionSet?

- [ ] **Step 2: Add `parse_selection` function**

Uses `peek_then_choice::<_, U1>`:
- `Spread` → fragment spread or inline fragment (further dispatch via peek)
- Default → field

- [ ] **Step 3: Add `parse_selection_set` function**

`{` Selection+ `}` using `repeated_while` checking for non-`}` tokens.

- [ ] **Step 4: Add `parse_fragment_definition` and `parse_inline_fragment` functions**

Generic versions in fragment.rs.

- [ ] **Step 5: Verify and commit**

---

### Task 4: Add generic type system parsers with peek_then_choice

**Files:**
- Modify: `smear-parser/src/graphql/ast/type_system.rs`

- [ ] **Step 1: Add individual type definition parsers**

Generic parsers for each type definition kind:
- `pub fn parse_scalar_type_definition`
- `pub fn parse_object_type_definition`
- `pub fn parse_interface_type_definition`
- `pub fn parse_union_type_definition`
- `pub fn parse_enum_type_definition`
- `pub fn parse_input_object_type_definition`
- `pub fn parse_directive_definition`
- `pub fn parse_schema_definition`

Each is a self-contained generic function.

- [ ] **Step 2: Add `parse_type_definition` with peek_then_choice**

Dispatch on keyword identifier using `peek_then_choice::<_, U1>`:
- "type" → object, "interface" → interface, "union" → union, etc.

- [ ] **Step 3: Add `parse_type_extension` with peek_then_choice**

Similar dispatch for `extend` keyword followed by type keyword.

- [ ] **Step 4: Verify and commit**

---

### Task 5: Add generic operation and document parsers

**Files:**
- Modify: `smear-parser/src/graphql/ast/default.rs`
- Modify: `smear-parser/src/graphql/ast.rs`

- [ ] **Step 1: Add operation parsers**

- `pub fn parse_operation_definition` — peek_then_choice: `{` → shorthand query, keyword → named operation
- `pub fn parse_variable_definition` — `$Name : Type DefaultValue? Directives?`
- `pub fn parse_variables_definition` — optional `( VariableDefinition+ )`

- [ ] **Step 2: Add `parse_executable_definition` with peek_then_choice**

Dispatch: `fragment` keyword → fragment, else → operation.

- [ ] **Step 3: Add `parse_definition` with peek_then_choice**

Full document definition: operation, fragment, type definition, type extension, schema.

- [ ] **Step 4: Add `parse_document`, `parse_executable_document`, `parse_type_system_document`**

Top-level parsers using `repeated_while` to collect definitions.

- [ ] **Step 5: Verify and commit**

---

### Task 6: Replace parse_str_impl.rs with generic ParseStr

**Files:**
- Delete: `smear-parser/src/graphql/ast/parse_str_impl.rs`
- Modify: `smear-parser/src/graphql/ast.rs`

- [ ] **Step 1: Delete parse_str_impl.rs**

Remove the file entirely.

- [ ] **Step 2: Implement ParseStr using generic parsers**

```rust
impl<'a> ParseStr<'a> for Document<&'a str> {
  fn parse_str(input: &'a str) -> Result<Self, SyntacticTokenErrors<&'a str>> {
    run_parse_str(parse_document, input)
  }
}
// ... for each type that had ParseStr
```

- [ ] **Step 3: Run tests**

Run: `cargo test --workspace --tests`
All 49 integration tests should pass.

- [ ] **Step 4: Commit**

---

### Task 7: Clean up exports and public API

**Files:**
- Modify: `smear-parser/src/graphql/ast.rs`
- Modify: `smear-parser/src/graphql/ast/default.rs`
- Modify: `smear-parser/src/lib.rs`

- [ ] **Step 1: Ensure all atomic parsers are `pub`**

Every parser function in:
- name.rs, keyword.rs, punctuator.rs, location.rs, operation_type.rs
- value.rs, value/*.rs
- ty.rs, fragment.rs, type_system.rs
- default/field.rs
- combinators.rs

Must be `pub fn` and re-exported through the module tree.

- [ ] **Step 2: Add `pub use` re-exports in ast.rs and default.rs**

Make all parsers accessible from `smear_parser::graphql::ast::*`.

- [ ] **Step 3: Run full test suite and benchmarks**

Run: `cargo test --workspace` and `cargo bench --package smear --bench simple_object -- --quick`

- [ ] **Step 4: Commit**

---

### Task 8: Apply same changes to GraphQLx parsers

**Files:**
- All files in `smear-parser/src/graphqlx/ast/`

- [ ] **Step 1: Apply the same patterns from Tasks 1-7 to GraphQLx**

GraphQLx parsers follow identical patterns but with extended token types. Apply the same generic combinator approach.

- [ ] **Step 2: Verify and commit**

---

### Task 9: Run benchmarks and verify no performance regression

- [ ] **Step 1: Run full benchmark suite**

```bash
cargo bench --package smear --bench simple_object -- --quick
cargo bench --package smear --bench gitlab_schema -- --quick
```

- [ ] **Step 2: Compare results with previous branch**

Verify performance is equal or better. The use of `peek_then_choice` should reduce redundant peeking.

- [ ] **Step 3: Final commit**
