//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute, an
//! `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so text
//! fidelity holds regardless.
//!
//! # What is here and what is in the substrate
//!
//! Everything below is **table**: the head sets an "expected one of" diagnostic names, this
//! dialect's balanced pairs, and its depth-zero restart predicate. The *logic* — report, skip,
//! attribute, guarantee progress — is [`crate::lossless::recover`]'s, shared with GraphQL, and its
//! module docs carry the reasoning about `sync_balanced`'s two no-progress cases and the
//! termination rule that follows from them.
//!
//! # The one place this dialect's tables genuinely differ: `<>` is a fourth balanced pair
//!
//! GraphQL's `delimiters` classifier has three pairs. GraphQLx's has four, because its lexer
//! depth-counts `<` and `>` alongside the other three (`increase_recursion!` / `decrease_recursion!`
//! on `b'<'` and `b'>'`, `smear-lexer/src/graphqlx/syntactic/mod.rs:807-814`). Without the fourth
//! pair a recovery inside `type T<A` would treat the `<` as neutral and the *first* `>` it met at
//! any nesting as an ordinary token, so a skip that started inside a generic list would run past
//! the list's own closer. The pair is not decoration: it is what makes recovery inside the whole
//! generics area terminate where the grammar does.
//!
//! The wrappers at the bottom exist so a production still writes
//! `unexpected::<Src, Ctx>(inp, VALUE_HEADS)` rather than a five-argument call naming this
//! dialect's tables at every site. They add no behaviour, exactly as `trivia.rs`'s do.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{
  SimpleSpan,
  error::{UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen},
  input::Balance,
};

use crate::graphqlx::{GraphQLx, kinds::SyntaxKind as K};

use super::trivia::kind_of;

/// The token kinds a `Value` may begin with — what an "expected a value" diagnostic names.
///
/// `true`, `false`, `null`, `set` and `map` are `Identifier`s to the lexer, so none of them needs
/// an entry of its own; the value dispatcher separates them on their spelling, not on their kind.
///
/// **`PathSeparator` is here and is not in GraphQL's set.** A bare `::` opens a fully qualified
/// enum value (`::ns::Colour`), which is a value head this dialect has and the other does not.
pub(crate) const VALUE_HEADS: &[Kind] = &[
  Kind::Dollar,
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
  Kind::PathSeparator,
];

/// The token kinds a `Value[Const]` may begin with — [`VALUE_HEADS`] without the `$`.
///
/// `VariableValue` is not a production of `Value[Const]` at all, so a const position expects
/// everything a value position does *except* a variable. The two sets are written out rather than
/// filtered because both are consumed as `&'static [Kind]`, and a `const fn` filter over a slice
/// cannot produce one.
pub(crate) const CONST_VALUE_HEADS: &[Kind] = &[
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
  Kind::PathSeparator,
];

/// The token kinds an `ObjectField` may begin with.
///
/// A field's key is a plain [`Name`](crate::graphqlx::kinds::SyntaxKind::Name), **not** a path:
/// `graphqlx/syntactic/value.rs`'s `object_field` is `name then_ignore(colon) then(value)`. Only
/// the *value* side widened.
pub(crate) const OBJECT_FIELD_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Type` may begin with — **four**, against GraphQL's two.
///
/// A path's name, a path's leading `::`, a list type's `[`, or a set/map type's `<`
/// (`graphqlx/syntactic/ty.rs:42-50`). The `!` is not among them: it is a *suffix* folded into the
/// node it follows, so no type reference ever starts with one, and a `!` in head position is
/// exactly the mistake this set names.
pub(crate) const TYPE_HEADS: &[Kind] = &[
  Kind::Identifier,
  Kind::PathSeparator,
  Kind::LBracket,
  Kind::LAngle,
];

/// The token kinds a `Path` — and therefore a `TypePath` — may begin with.
pub(crate) const PATH_HEADS: &[Kind] = &[Kind::Identifier, Kind::PathSeparator];

/// The token kinds an `Argument` may begin with.
///
/// An argument's key is a plain [`Name`](crate::graphqlx::kinds::SyntaxKind::Name), the same
/// ruling [`OBJECT_FIELD_HEADS`] records: `graphqlx/syntactic/argument/mod.rs`'s `argument` is
/// `take_argument_name then_ignore(colon) then(value)`, so only the value side widened.
pub(crate) const ARGUMENT_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Selection` may begin with.
pub(crate) const SELECTION_HEADS: &[Kind] = &[Kind::Identifier, Kind::Spread];

/// The token kinds that may follow a `...` — **four**, against GraphQL's three.
///
/// `PathSeparator` is the extra one and it is a whole arm rather than a wider name: a `...` before
/// a `::` opens a fully qualified fragment spread (`graphqlx/syntactic/selection/mod.rs:427-435`).
pub(crate) const SPREAD_TAIL_HEADS: &[Kind] = &[
  Kind::Identifier,
  Kind::PathSeparator,
  Kind::At,
  Kind::LBrace,
];

/// The token kinds a `TypeCondition` may begin with.
///
/// `on` is an `Identifier` to the lexer, and the type after it is a
/// [`TypePath`](crate::graphqlx::kinds::SyntaxKind::TypePath) rather than GraphQL's bare named
/// type — so a leading `::` is a head here and is not one in GraphQL's set.
pub(crate) const TYPE_CONDITION_HEADS: &[Kind] = &[Kind::Identifier, Kind::PathSeparator];

/// The token kinds a `VariableDefinition` may begin with.
///
/// **Three, against GraphQL's one.** Divergence 12: a GraphQLx variable definition may carry a
/// leading description (`graphqlx/syntactic/executable/mod.rs:343-346`), so the two string kinds
/// are heads here and the `$` is only the most common one.
pub(crate) const VARIABLE_DEFINITION_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Dollar];

/// The token kinds an `ExecutableDefinition` may begin with: a description, the shorthand's `{`,
/// or one of the four keyword spellings, all of which are `Identifier`s.
pub(crate) const EXECUTABLE_DEFINITION_HEADS: &[Kind] = &[
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds a bare `Name` position admits — an operation's name, a generic parameter, a
/// fragment's name, a directive location.
pub(crate) const NAME_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds an operation type may begin with.
///
/// [`NAME_HEADS`]'s set under its own name: `query`, `mutation` and `subscription` are ordinary
/// identifiers, so the *kind* answers nothing and only a spelling test can — which is why the
/// diagnostic naming this set is always accompanied by one.
pub(crate) const OPERATION_TYPE_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a described SDL member may begin with: its description, or its own name.
pub(crate) const DESCRIBED_MEMBER_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The token kinds a type-system definition may begin with — a description, or one of the eight
/// keyword spellings, all of which are `Identifier`s.
pub(crate) const TYPE_SYSTEM_DEFINITION_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The `{` a schema definition's root-operation block must open with.
pub(crate) const ROOT_OPERATION_TYPES_HEADS: &[Kind] = &[Kind::LBrace];

/// The `{` a fields or input-fields block must open with — divergence 16's expectation, at the
/// three definition sites where a present `where` makes the block mandatory.
pub(crate) const FIELDS_BLOCK_HEADS: &[Kind] = &[Kind::LBrace];

/// The token kinds an import member may begin with — divergence 21: a name, or the wildcard `*`.
pub(crate) const IMPORT_MEMBER_HEADS: &[Kind] = &[Kind::Identifier, Kind::Asterisk];

/// The token kinds an import clause may begin with: the wildcard, or a braced list.
pub(crate) const IMPORT_CLAUSE_HEADS: &[Kind] = &[Kind::Asterisk, Kind::LBrace];

/// The one token kind an import's `from` source may be.
///
/// **Inline only.** `import_definition_after_keyword` calls `inline_string_value`, so a block
/// string is a grammar error there even though the node above it is a `StringValue` either way.
pub(crate) const IMPORT_SOURCE_HEADS: &[Kind] = &[Kind::InlineString];

/// The `=` a union's members must open with — divergence 17's expectation, which is `Equal` and
/// **not** `LBrace`: a union's `where` requires the members *before* it where an object's requires
/// the block *after* it.
pub(crate) const UNION_MEMBERS_HEADS: &[Kind] = &[Kind::Equal];

/// Whether `kind` can begin a value.
///
/// [`VALUE_HEADS`] as a predicate. The two are kept in step by
/// `the_value_head_set_and_predicate_agree` in `tests/lossless_x_value.rs` rather than by one being
/// derived from the other: the set is consumed as a `&'static [Kind]` by the diagnostic path and a
/// `const fn` cannot filter a slice into another one, and the predicate is consumed by a `match`
/// guard, which cannot read a slice at all without a linear scan on every member of every
/// collection literal.
///
/// The **non**-const set is the right one even inside a const collection: a `$` in a const position
/// is reported by `value` and then parsed anyway, so a member loop that refused to enter `value`
/// on a `$` would turn a diagnostic into a skipped region.
#[inline]
pub(crate) const fn starts_value(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Dollar
      | Kind::Int
      | Kind::Float
      | Kind::InlineString
      | Kind::BlockString
      | Kind::LBracket
      | Kind::LBrace
      | Kind::Identifier
      | Kind::PathSeparator
  )
}

/// Whether `kind` can begin a type reference — [`TYPE_HEADS`] as a predicate, kept in step with the
/// set by the same test that pins [`starts_value`] against [`VALUE_HEADS`].
#[inline]
pub(crate) const fn starts_type(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Identifier | Kind::PathSeparator | Kind::LBracket | Kind::LAngle
  )
}

// `starts_description` — the one two-kind test every described position makes. Generated from
// Task 7's frozen macro rather than written out, so a dialect cannot forget `BlockString` in one
// of the places it asks. It lives here beside `starts_value` and `starts_type` because it is the
// same kind of thing: a head predicate over the lexer's vocabulary, consumed by a `match` guard.
//
// GraphQLx asks it in more places than GraphQL does — divergence 12 puts a description on a
// variable definition and on an executable definition, neither of which GraphQL has.
crate::lossless::description_head_predicate!(smear_lexer::graphqlx::lossless::LosslessTokenKind);

pub(crate) use crate::lossless::recover::opener_span;

/// Where a recovery is willing to stop: a token that could start something the caller knows how to
/// parse, or a closer the enclosing shape knows how to consume.
///
/// The closers are in the set even though nothing starts with one — stopping *before* `]` is what
/// lets an enclosing `list_value` close on its own delimiter instead of running to end of input.
/// `sync_balanced` consults this only at depth zero, so a `]` inside skipped nesting is crossed
/// rather than mistaken for the enclosing one.
///
/// # One set, not one per position
///
/// The set is deliberately the **union** of every head this suite can restart at, rather than the
/// caller's own head set. A recovery that stopped only at heads the *current* production accepts
/// would run past the closer that ends it, and past the head of the sibling that follows — so a
/// stray token inside a field would cost the rest of the selection set. Stopping early costs at
/// most one extra `Error` node; stopping late costs a subtree.
///
/// # What this dialect adds to GraphQL's set, and what it deliberately does not
///
/// `PathSeparator` and `RAngle` are in; `LAngle`, `FatArrow`, `Asterisk`, `Plus` and `Minus` are
/// out. A `::` starts a fully qualified path, which is a value and a type head, so a skip that ran
/// past one would fold a whole qualified name into the junk before it. A `>` closes a generic list,
/// a set type and a map type — the same argument the other three closers make. The five that stay
/// out begin no production this suite can restart at: `<` only ever *follows* something (a path, or
/// the type position itself, which `LBracket`/`Identifier`/`PathSeparator` already cover as heads),
/// `=>` only ever separates, and `*`/`+`/`-` reach the tree through recovery alone. Stopping at one
/// of those would only split one `Error` node into two.
#[inline]
fn is_sync_point(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Dollar
      | Kind::Int
      | Kind::Float
      | Kind::InlineString
      | Kind::BlockString
      | Kind::LBracket
      | Kind::LBrace
      | Kind::Identifier
      | Kind::PathSeparator
      | Kind::Spread
      | Kind::RBracket
      | Kind::RBrace
      | Kind::RParen
      | Kind::RAngle
  )
}

/// GraphQLx's **four** delimiter pairs, for `sync_balanced`'s depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because the balanced scan is deliberately
/// pair-blind — it counts depth and never checks that a closer matches its opener — so the identity
/// is only ever compared for equality, and never by this suite.
///
/// The `<>` row is the difference from GraphQL's classifier, and the module docs say why dropping
/// it would break recovery for the whole generics area rather than merely coarsening it.
#[inline]
fn delimiters(kind: &Kind) -> Balance<u8> {
  match kind {
    Kind::LParen => Balance::Open(b'('),
    Kind::RParen => Balance::Close(b'('),
    Kind::LBracket => Balance::Open(b'['),
    Kind::RBracket => Balance::Close(b'['),
    Kind::LBrace => Balance::Open(b'{'),
    Kind::RBrace => Balance::Close(b'{'),
    Kind::LAngle => Balance::Open(b'<'),
    Kind::RAngle => Balance::Close(b'<'),
    _ => Balance::Neutral,
  }
}

// ---------------------------------------------------------------------------------------------
// The wrappers. Each one binds this dialect's tables to `crate::lossless::recover`'s logic and adds
// nothing else; the `lossless_production!` bundle is what makes them nameable from a production
// without a turbofish per argument.
//
// The substrate's restart predicate takes `fn(&L::Token) -> bool`, never `fn(Kind) -> bool`, so
// that it never has to name a kind space. The adapter is a non-capturing closure at the call site
// rather than a named item: written as a `fn` it would need the token's `Token<'a>` bound restated,
// because a lossless `Token` impl is generated per concrete slice type — and inside a
// `lossless_production!` body that bound is already in scope.
// ---------------------------------------------------------------------------------------------

use crate::lossless::lossless_production;

lossless_production! {
  dialect = graphqlx::lossless;

  /// A list ran to end of input before its `]` arrived. [`crate::lossless::recover::unclosed`]
  /// with this pair's marker.
  fn unclosed_list<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(
      inp,
      UnclosedBracket::<SimpleSpan, GraphQLx>::bracket_of(open),
    )
  }

  /// A brace-delimited value — an object, a `set { … }` or a `map { … }` — ran to end of input
  /// before its `}` arrived.
  fn unclosed_object<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedBrace::<SimpleSpan, GraphQLx>::brace_of(open))
  }

  /// A parenthesised list — an argument list or a variables definition — ran to end of input
  /// before its `)` arrived.
  fn unclosed_parens<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedParen::<SimpleSpan, GraphQLx>::paren_of(open))
  }

  /// An angle-delimited shape — a set type, a map type or a generic list — ran to end of input
  /// before its `>` arrived.
  ///
  /// **The pair GraphQL has no counterpart for**, and the reason
  /// [`FromUnclosed`](tokora::emitter::FromUnclosed) is generic over the delimiter marker: one impl
  /// covers all four pairs, so this one costs a constructor call and no new bound. It reaches
  /// `Unclosed::Angle` rather than the catch-all's untyped note because `lossless/mod.rs` lists
  /// `"<>"` — pinned by `tests/lossless_x_errors.rs`.
  fn unclosed_angle<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedAngle::<SimpleSpan, GraphQLx>::angle_of(open))
  }

  /// Nothing that could start one of `expected` is here. **Report, and consume nothing.**
  /// [`crate::lossless::recover::report_unexpected`]; that function's docs say when to reach for it
  /// rather than for [`unexpected`], and each of the shapes it serves is a bug if it consumes.
  fn report_unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    crate::lossless::recover::report_unexpected(inp, expected)
  }

  /// Nothing that could start one of `expected` is here, and there is still input.
  /// [`crate::lossless::recover::unexpected`] with this dialect's four pairs and its restart
  /// predicate.
  fn unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    crate::lossless::recover::unexpected(
      inp,
      expected,
      delimiters,
      |t| is_sync_point(kind_of(t)),
      K::Error.raw(),
    )
  }
}
