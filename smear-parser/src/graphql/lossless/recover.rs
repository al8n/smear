//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute,
//! an `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so
//! text fidelity holds regardless.
//!
//! # What is here and what is in the substrate
//!
//! Everything below is **table**: the twenty-one head sets an "expected one of" diagnostic names,
//! this dialect's three balanced pairs, its depth-zero restart predicate and its definition-start
//! predicate. The *logic* — report, skip, attribute, guarantee progress — is
//! [`crate::lossless::recover`]'s, shared with every dialect, and its module docs carry the
//! reasoning about `sync_balanced`'s two no-progress cases and the termination rule that follows
//! from them.
//!
//! The wrappers at the bottom exist so a production still writes
//! `unexpected::<Src, Ctx>(inp, VALUE_HEADS)` rather than a five-argument call naming this
//! dialect's tables at every one of ~40 sites. They add no behaviour, exactly as `trivia.rs`'s do.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{
  SimpleSpan,
  error::{UnclosedBrace, UnclosedBracket, UnclosedParen},
  input::Balance,
  utils::DowncastRef,
};

use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

use super::trivia::kind_of;

/// The token kinds a `Value` may begin with — what an "expected a value" diagnostic names.
///
/// `true`, `false` and `null` are `Identifier`s to the lexer, so they need no entry of their
/// own; the value dispatcher separates them on their spelling, not on their kind.
pub(crate) const VALUE_HEADS: &[Kind] = &[
  Kind::Dollar,
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds a `Value[Const]` may begin with — [`VALUE_HEADS`] without the `$`.
///
/// `Variable` is not a production of `Value[Const]` at all, so a const position expects
/// everything a value position does *except* a variable. The two sets are written out rather
/// than filtered because both are consumed as `&'static [Kind]`, and a `const fn` filter over a
/// slice cannot produce one.
pub(crate) const CONST_VALUE_HEADS: &[Kind] = &[
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds an `ObjectField` may begin with.
pub(crate) const OBJECT_FIELD_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Type` may begin with: a name, or the `[` of a list type.
///
/// The `!` of a `NonNullType` is not among them — it is a *suffix*, so no type reference ever
/// starts with one, and a `!` in head position is exactly the mistake this set names.
pub(crate) const TYPE_HEADS: &[Kind] = &[Kind::Identifier, Kind::LBracket];

/// The token kinds an `Argument` may begin with.
pub(crate) const ARGUMENT_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Selection` may begin with: a field's name, or the `...` of a fragment
/// spread or inline fragment.
pub(crate) const SELECTION_HEADS: &[Kind] = &[Kind::Identifier, Kind::Spread];

/// The token kinds that may follow a `...`: a fragment name or the `on` of a type condition
/// (both `Identifier`s), a directive's `@`, or the `{` of an untyped inline fragment's
/// selection set.
pub(crate) const SPREAD_TAIL_HEADS: &[Kind] = &[Kind::Identifier, Kind::At, Kind::LBrace];

/// The token kinds a type condition may begin with — one, and it stands for two positions.
///
/// `on` and the name after it are both `Identifier`s, and `expectation_of` collapses every
/// token-kind expectation onto [`Expectation::Name`](crate::graphql::error::Expectation::Name)
/// anyway, so a second set naming the keyword would report the same sentence. The precise
/// "expected the keyword `on`" wording needs the dialect error rather than tokora's
/// token-kind one, and no production here carries the bound that would reach it.
pub(crate) const TYPE_CONDITION_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `VariableDefinition` may begin with.
pub(crate) const VARIABLE_DEFINITION_HEADS: &[Kind] = &[Kind::Dollar];

/// The token kinds an executable definition may begin with: the leading string of a
/// description, the `{` of a shorthand operation, or the
/// `query`/`mutation`/`subscription`/`fragment` keyword — an `Identifier` to the lexer, which
/// is why the set cannot be finer than this.
pub(crate) const EXECUTABLE_DEFINITION_HEADS: &[Kind] = &[
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds a described SDL member may begin with: its own name, or the leading string
/// of a description.
///
/// One set for `FieldDefinition`, `InputValueDefinition` and `EnumValueDefinition` — all three
/// are `Description? Name …`, so they open on exactly the same three kinds and a set per
/// production would name the same three kinds three times.
pub(crate) const DESCRIBED_MEMBER_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The token kinds a `NamedType` may begin with — one, and it stands for five positions: an
/// interface in an `implements` clause, a member of a union, a root operation type's target, a
/// directive location, and a `FragmentName`.
///
/// The last two are *spelling* rules — `DirectiveLocation` admits nineteen names and
/// `FragmentName` is `Name but not "on"` — which this kind-level set cannot express and does not
/// try to: `lossless/mod.rs`'s `expectation_of` collapses every token-kind expectation onto
/// [`Expectation::Name`](crate::graphql::error::Expectation::Name) anyway, so a finer set here
/// would report the same sentence. The precise wording needs the dialect error rather than
/// tokora's token-kind one, and no production here carries the bound that would reach it — the
/// ruling [`TYPE_CONDITION_HEADS`] already records for the keyword `on`.
pub(crate) const NAME_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `RootOperationTypeDefinition` may begin with: the `query`, `mutation` or
/// `subscription` keyword, all `Identifier`s to the lexer.
pub(crate) const OPERATION_TYPE_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds any definition may begin with — the top-level dispatch's set, and the widest
/// in this file: a description's string, the `{` of a shorthand operation, or a keyword.
pub(crate) const DEFINITION_HEADS: &[Kind] = &[
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds a type-system definition or extension may begin with — [`DEFINITION_HEADS`]
/// without the shorthand operation's `{`, since no type-system definition is anonymous.
pub(crate) const TYPE_SYSTEM_DEFINITION_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The token kinds that may follow an `extend`: the shape keyword, an `Identifier` to the
/// lexer.
pub(crate) const TYPE_EXTENSION_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds that may open a `ScalarTypeExtension`'s tail. The grammar gives it exactly
/// one shape — `extend scalar Name Directives` — so its directives are mandatory where every
/// other definition's are optional.
pub(crate) const SCALAR_EXTENSION_TAIL_HEADS: &[Kind] = &[Kind::At];

/// The token kinds that may open an `ObjectTypeExtension`'s or `InterfaceTypeExtension`'s tail:
/// `implements` (an `Identifier`), a directive's `@`, or a fields definition's `{`.
pub(crate) const OBJECT_EXTENSION_TAIL_HEADS: &[Kind] = &[Kind::Identifier, Kind::At, Kind::LBrace];

/// The token kinds that may open a `UnionTypeExtension`'s tail: a directive's `@` or the `=` of
/// a union member list.
pub(crate) const UNION_EXTENSION_TAIL_HEADS: &[Kind] = &[Kind::At, Kind::Equal];

/// The token kinds that may open an `EnumTypeExtension`'s, `InputObjectTypeExtension`'s or
/// `SchemaExtension`'s tail: a directive's `@` or the `{` of the block that follows.
pub(crate) const BLOCK_EXTENSION_TAIL_HEADS: &[Kind] = &[Kind::At, Kind::LBrace];

/// The token kinds a `SchemaDefinition`'s root-operation block may begin with.
pub(crate) const ROOT_OPERATION_TYPES_HEADS: &[Kind] = &[Kind::LBrace];

pub(crate) use crate::lossless::recover::opener_span;

/// Where a recovery is willing to stop: a token that could start something the caller knows
/// how to parse, or a closer the enclosing shape knows how to consume.
///
/// The closers are in the set even though nothing starts with one — stopping *before* `]` is
/// what lets an enclosing `list_value` close on its own delimiter instead of running to end of
/// input. `sync_balanced` consults this only at depth zero, so a `]` inside skipped nesting is
/// crossed rather than mistaken for the enclosing one.
///
/// # One set, not one per position
///
/// The set is deliberately the **union** of every head this suite can restart at, rather than
/// the caller's own head set. A recovery that stopped only at heads the *current* production
/// accepts would run past the closer that ends it, and past the head of the sibling that
/// follows — so a stray token inside a field would cost the rest of the selection set. Stopping
/// early costs at most one extra `Error` node; stopping late costs a subtree.
///
/// **`Spread` was added in Task 7 and is the only head that is not also a value head.** A
/// `...` starts a selection, so a set that stopped only at names would skip straight over a
/// fragment spread and fold it into the junk before it — pinned by
/// `junk_before_a_spread_does_not_swallow_it`. `At`, `Colon` and `Bang` stay out: none of them
/// begins a production this suite can restart at, so stopping there would only split one
/// `Error` node into two.
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
      | Kind::Spread
      | Kind::RBracket
      | Kind::RBrace
      | Kind::RParen
  )
}

/// Whether `token` begins a top-level definition — the predicate the **catch** arm of
/// [`document`](super::document::document) resynchronises to, and a strictly narrower set than
/// [`is_sync_point`].
///
/// # Why not `is_sync_point`
///
/// The two arms recover from different situations and want different answers. An *unrecognised
/// head* means the parser has not started anything yet, so the widest set that can restart
/// anything at all is right — that is [`unexpected`]'s. A *production `Err`* means a definition
/// was half-parsed and its remaining body is wreckage; stopping at the first name inside that
/// body would hand the document loop a `type`-less field to re-fail on, one token at a time.
/// Skipping to the next keyword costs one `Error` node for the whole wreck.
///
/// # The shorthand `{` is deliberately absent
///
/// `{` opens a shorthand operation and so *is* a definition head — but including it here would
/// make the scan stop on the very brace that usually opens the wreckage (`type { … }`),
/// returning a zero-skip hole and degrading this helper to a consume-one loop. A shorthand
/// operation after junk is still found, by the other arm: [`is_sync_point`] carries `LBrace`.
///
/// A description's string is present, because a string is where a described definition starts
/// and the next loop turn can parse one.
#[inline]
fn is_definition_start(kind: Kind, keyword: Option<ContextualKeyword>) -> bool {
  match kind {
    Kind::InlineString | Kind::BlockString => true,
    Kind::Identifier => matches!(
      keyword,
      Some(
        ContextualKeyword::Query
          | ContextualKeyword::Mutation
          | ContextualKeyword::Subscription
          | ContextualKeyword::Fragment
          | ContextualKeyword::Schema
          | ContextualKeyword::Directive
          | ContextualKeyword::Scalar
          | ContextualKeyword::Type
          | ContextualKeyword::Interface
          | ContextualKeyword::Union
          | ContextualKeyword::Enum
          | ContextualKeyword::Input
          | ContextualKeyword::Extend
      )
    ),
    _ => false,
  }
}

/// [`crate::lossless::recover::keyword_of`] with this dialect's keyword projection pinned.
///
/// The wrapper exists so the two predicates below read as membership tests over a named enum
/// rather than as turbofished downcasts.
#[inline]
fn keyword_of<T: DowncastRef<ContextualKeyword>>(token: &T) -> Option<ContextualKeyword> {
  crate::lossless::recover::keyword_of(token)
}

/// GraphQL's three delimiter pairs, for `sync_balanced`'s depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because the balanced scan is
/// deliberately pair-blind — it counts depth and never checks that a closer matches its
/// opener — so the identity is only ever compared for equality, and never by this suite.
#[inline]
fn delimiters(kind: &Kind) -> Balance<u8> {
  match kind {
    Kind::LParen => Balance::Open(b'('),
    Kind::RParen => Balance::Close(b'('),
    Kind::LBracket => Balance::Open(b'['),
    Kind::RBracket => Balance::Close(b'['),
    Kind::LBrace => Balance::Open(b'{'),
    Kind::RBrace => Balance::Close(b'{'),
    _ => Balance::Neutral,
  }
}

// ---------------------------------------------------------------------------------------------
// The wrappers. Each one binds this dialect's tables to `crate::lossless::recover`'s logic and
// adds nothing else; the `lossless_production!` bundle is what makes them nameable from a
// production without a turbofish per argument.
// ---------------------------------------------------------------------------------------------

// The substrate's two restart predicates take `fn(&L::Token) -> bool`, never `fn(Kind) -> bool`,
// so that it never has to name a kind space. The adapters are non-capturing closures at the two
// call sites below rather than named items: written as `fn`s they would need the token's
// `Token<'a>` and `DowncastRef` bounds restated, because a lossless `Token` impl is generated per
// concrete slice type — and inside a `lossless_production!` body those bounds are already in
// scope.

use crate::lossless::lossless_production;

lossless_production! {
  dialect = graphql::lossless;

  /// A list ran to end of input before its `]` arrived. [`crate::lossless::recover::unclosed`]
  /// with this pair's marker.
  fn unclosed_list<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(
      inp,
      UnclosedBracket::<SimpleSpan, GraphQL>::bracket_of(open),
    )
  }

  /// An object ran to end of input before its `}` arrived.
  fn unclosed_object<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedBrace::<SimpleSpan, GraphQL>::brace_of(open))
  }

  /// An argument list ran to end of input before its `)` arrived — and the reason
  /// [`FromUnclosed`] is generic over the delimiter marker: one impl covers `[]`, `{}` and `()`,
  /// so a new pair costs a constructor call and no new bound.
  fn unclosed_parens<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedParen::<SimpleSpan, GraphQL>::paren_of(open))
  }

  /// Nothing that could start one of `expected` is here. **Report, and consume nothing.**
  /// [`crate::lossless::recover::report_unexpected`]; that function's docs say when to reach for
  /// it rather than for [`unexpected`], and each of the three shapes is a bug if it consumes.
  fn report_unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    crate::lossless::recover::report_unexpected(inp, expected)
  }

  /// Nothing that could start one of `expected` is here, and there is still input.
  /// [`crate::lossless::recover::unexpected`] with this dialect's pairs and restart predicate.
  fn unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    crate::lossless::recover::unexpected(
      inp,
      expected,
      delimiters,
      |t| is_sync_point(kind_of(t)),
      K::Error.raw(),
    )
  }

  /// A definition returned `Err`: skip its wreckage and stop before the next definition head.
  /// [`crate::lossless::recover::resync_to`] with this dialect's definition-start predicate,
  /// which is deliberately narrower than [`is_sync_point`] — see both predicates' docs.
  fn resync_to_definition<'inp, Src, Ctx>(inp) {
    crate::lossless::recover::resync_to(
      inp,
      delimiters,
      |t| is_definition_start(kind_of(t), keyword_of(t)),
      K::Error.raw(),
    )
  }
}
