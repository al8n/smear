//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute, an
//! `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so text
//! fidelity holds regardless.
//!
//! # What is here and what is in the substrate
//!
//! Everything below is **table**: the head sets an "expected one of" diagnostic names, this
//! dialect's balanced pairs, and its restart predicate. The *logic* — report, skip,
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
//! dialect's tables at every site. They add no behaviour, exactly as `trivia.rs`'s do — with the
//! two a module's body adds to the set: `skip_mod_junk` is the substrate's report followed by its
//! resync, one call where the body would write two, and `skip_one` is the one-token step every
//! helper there falls back to, spelled where a loop can take it by itself.

use smear_lexer::graphqlx::lossless::LosslessTokenKind as Kind;
use tokora::{
  SimpleSpan,
  error::{UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen},
  input::Balance,
};

use crate::graphqlx::{GraphQLx, kinds::SyntaxKind as K};

use super::{Keyword, trivia::kind_of};
use crate::lossless::recover::keyword_of;

/// The token kinds a `Value` may begin with — what an "expected a value" diagnostic names.
///
/// `true`, `false`, `null`, `set` and `map` are `Identifier`s to the lexer, so none of them needs
/// an entry of its own; the value dispatcher separates them on their spelling, not on their kind.
///
/// **`Tilde`, `DotDot` and `Caret` are here and are not in GraphQL's set.** A root's sigil opens a
/// rooted enum value (`^::ns::Colour`), which is a value head this dialect has and the other does
/// not. `PathSeparator` is not: a `::` only joins, so no value starts with one.
pub(crate) const VALUE_HEADS: &[Kind] = &[
  Kind::Dollar,
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBracket,
  Kind::LBrace,
  Kind::Identifier,
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
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
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
];

/// The token kinds an `ObjectField` may begin with.
///
/// A field's key is a plain [`Name`](crate::graphqlx::kinds::SyntaxKind::Name), **not** a path:
/// `graphqlx/syntactic/value.rs`'s `object_field` is `name then_ignore(colon) then(value)`. Only
/// the *value* side widened.
pub(crate) const OBJECT_FIELD_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Type` may begin with — **five**, against GraphQL's two.
///
/// A path's name, one of its root sigils `~`, `..` and `^`, or a list type's `[`
/// (`graphqlx/syntactic/ty.rs:38-49`). The `!` is not among them: it is a *suffix* folded into the
/// node it follows, so no type reference ever starts with one, and a `!` in head position is
/// exactly the mistake this set names. Nor is `::`, which only joins two segments.
pub(crate) const TYPE_HEADS: &[Kind] = &[
  Kind::Identifier,
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
  Kind::LBracket,
];

/// The token kinds a `Path` — and therefore a `TypePath` — may begin with: a name, or one of the
/// three root sigils.
pub(crate) const PATH_HEADS: &[Kind] = &[Kind::Identifier, Kind::Tilde, Kind::DotDot, Kind::Caret];

/// The token kinds an `Argument` may begin with.
///
/// An argument's key is a plain [`Name`](crate::graphqlx::kinds::SyntaxKind::Name), the same
/// ruling [`OBJECT_FIELD_HEADS`] records: `graphqlx/syntactic/argument/mod.rs`'s `argument` is
/// `take_argument_name then_ignore(colon) then(value)`, so only the value side widened.
pub(crate) const ARGUMENT_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `Selection` may begin with.
pub(crate) const SELECTION_HEADS: &[Kind] = &[Kind::Identifier, Kind::Spread];

/// The token kinds that may follow a `...` — **six**, against GraphQL's three.
///
/// The three root sigils are the extra ones and they are a whole arm rather than a wider name: a
/// `...` before `~`, `..` or `^` opens a rooted fragment spread
/// (`graphqlx/syntactic/selection/mod.rs:459-462`).
pub(crate) const SPREAD_TAIL_HEADS: &[Kind] = &[
  Kind::Identifier,
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
  Kind::At,
  Kind::LBrace,
];

/// The token kinds a `TypeCondition` may begin with.
///
/// `on` is an `Identifier` to the lexer, and the type after it is a
/// [`TypePath`](crate::graphqlx::kinds::SyntaxKind::TypePath) rather than GraphQL's bare named
/// type — so a root's sigil is a head here and is not one in GraphQL's set.
pub(crate) const TYPE_CONDITION_HEADS: &[Kind] =
  &[Kind::Identifier, Kind::Tilde, Kind::DotDot, Kind::Caret];

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

/// The token kinds that may follow a description on a definition: the keyword that opens the
/// described definition, and nothing else.
///
/// Every alternative carrying a `Description?` opens with a keyword — an `Identifier` to the
/// lexer. The one that does not is the shorthand `OperationDefinition : SelectionSet`, which is
/// why this set excludes the `{` that [`EXECUTABLE_DEFINITION_HEADS`] admits.
pub(crate) const DESCRIBED_DEFINITION_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a type-system definition may begin with — a description, or one of the eight
/// keyword spellings, all of which are `Identifier`s.
pub(crate) const TYPE_SYSTEM_DEFINITION_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The `{` a schema definition's root-operation block must open with.
pub(crate) const ROOT_OPERATION_TYPES_HEADS: &[Kind] = &[Kind::LBrace];

/// The `{` a fields or input-fields block must open with — divergence 16's expectation, at the
/// three definition sites where a present `where` makes the block mandatory.
pub(crate) const FIELDS_BLOCK_HEADS: &[Kind] = &[Kind::LBrace];

/// The token kinds a mixed document entry may begin with: a description, a `use` item, an
/// extension, a definition keyword, or the shorthand operation's `{`.
pub(crate) const DEFINITION_HEADS: &[Kind] = &[
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds an executable-document entry may begin with.
pub(crate) const EXECUTABLE_ENTRY_HEADS: &[Kind] = &[
  Kind::InlineString,
  Kind::BlockString,
  Kind::LBrace,
  Kind::Identifier,
];

/// The token kinds a type-system-document entry may begin with — no `{`, the shorthand operation
/// not being a type-system definition.
pub(crate) const TYPE_SYSTEM_ENTRY_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The token kinds a type-system extension's shape keyword may be.
pub(crate) const TYPE_EXTENSION_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds an extension's tail may begin with — the "at least one component" report.
///
/// Every one of `syntactic/`'s eight extension tails refuses an empty tail: seven through a
/// `(None, None, …) => Err` arm, and a directive extension by requiring its directives outright. A
/// bare `extend type T` re-prints as valid text, so the verdict is the only witness the rule has.
pub(crate) const EXTENSION_TAIL_HEADS: &[Kind] =
  &[Kind::At, Kind::LBrace, Kind::Equal, Kind::Identifier];

/// The token kinds that may follow `use`: a path's name, a root's sigil, or the inline string a
/// source is.
pub(crate) const USE_TREE_HEADS: &[Kind] = &[
  Kind::Identifier,
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
  Kind::InlineString,
];

/// The token kinds a use tree's `::`, or a bare root, continues into: a name, the glob `*`, or a
/// group's `{`.
pub(crate) const USE_SCOPE_HEADS: &[Kind] = &[Kind::Identifier, Kind::Asterisk, Kind::LBrace];

/// The token kinds a tree inside a group may begin with: a name, and nothing else — no root and no
/// string source.
pub(crate) const USE_MEMBER_HEADS: &[Kind] = &[Kind::Identifier];

/// The one token kind a string source may be.
///
/// **Inline only.** `graphqlx/syntactic/use_tree.rs` reads a source through `inline_string_value`,
/// so a block string is a grammar error there even though the node above it is a `StringValue`
/// either way.
pub(crate) const USE_SOURCE_HEADS: &[Kind] = &[Kind::InlineString];

/// What may follow a `use` item's glob or group: nothing of the item's own. The item ends there,
/// so an `as` after one is reported against no head at all.
pub(crate) const USE_ITEM_END_HEADS: &[Kind] = &[];

/// The `{` a module's body must open with, after its name.
///
/// Required: an empty body is written `{}`, and a module with no braces at all is a grammar error
/// in both suites.
pub(crate) const MOD_BODY_HEADS: &[Kind] = &[Kind::LBrace];

/// The token kinds an item of a module's body may begin with: a description, or the keyword of a
/// `use` item, a module, a type-system definition or an extension — all `Identifier`s.
///
/// The set a type-system document's entry admits, under its own name, because what it names is a
/// different position: inside a body a `}` ends the run, and an executable definition's keyword
/// starts no item at all.
pub(crate) const MOD_ITEM_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Identifier];

/// The `=` a union's members must open with — divergence 17's expectation, which is `Equal` and
/// **not** `LBrace`: a union's `where` requires the members *before* it where an object's requires
/// the block *after* it.
pub(crate) const UNION_MEMBERS_HEADS: &[Kind] = &[Kind::Equal];

/// The `)` a parenthesised list closes with: what such a list reports as expected where it stops
/// short of one, at a `}` that belongs to a block around it. See [`closes_enclosing_block`].
pub(crate) const PARENS_CLOSER_HEADS: &[Kind] = &[Kind::RParen];

/// The `]` a list type or a list value closes with, reported as [`PARENS_CLOSER_HEADS`] is.
pub(crate) const LIST_CLOSER_HEADS: &[Kind] = &[Kind::RBracket];

/// The `>` a generic list closes with, reported as [`PARENS_CLOSER_HEADS`] is.
pub(crate) const ANGLE_CLOSER_HEADS: &[Kind] = &[Kind::RAngle];

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
      | Kind::Tilde
      | Kind::DotDot
      | Kind::Caret
  )
}

/// Whether `kind` can begin a type reference — [`TYPE_HEADS`] as a predicate, kept in step with the
/// set by the same test that pins [`starts_value`] against [`VALUE_HEADS`].
#[inline]
pub(crate) const fn starts_type(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Identifier | Kind::Tilde | Kind::DotDot | Kind::Caret | Kind::LBracket
  )
}

/// Whether `kind` closes one of this dialect's four delimiter pairs.
///
/// Asked where the grammar requires exactly one component — a type after a `:`, a value after an
/// `=` — and the head starts none. A closer there is the closer of the block the position stands
/// in, so it is reported and left for that block's own loop, where every other head is junk to
/// consume. Consuming it would cost the block its end and everything after the block with it.
#[inline]
pub(crate) const fn is_closer(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::RParen | Kind::RBracket | Kind::RBrace | Kind::RAngle
  )
}

/// Whether a `}` at the head of a list delimited by `(`, `[` or `<` belongs to a block around the
/// list, and so is not the list's to consume.
///
/// `open_at` is the lexer's count of open braces right after the list's own opener was read, and
/// `now` is the count at the committed position: the `}` at the head is only peeked, so it is not
/// in `now` yet. The `}` closes a brace that was open before the list began exactly when one was
/// (`open_at > 0`) and the list's own content has left none of its own open (`now <= open_at`).
/// The list then reports its own closer as missing and returns, and the block's loop finds its `}`
/// where it stands. Taking it as junk would cost the block its end, and hand the list everything
/// after the block.
///
/// In the two other cases the `}` stays junk of the list. With no brace open before the list it
/// closes nothing and is a stray; with more open now than then it closes a brace the list's own
/// content opened. A `)`, `]` or `>` that is not the list's own is junk too, as it has always
/// been: no count tells a stray one from an enclosing one.
#[inline]
pub(crate) const fn closes_enclosing_block(open_at: usize, now: usize) -> bool {
  open_at > 0 && now <= open_at
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
/// The skip stops at this only where its depth over every pair is zero, so a `]` inside nesting it
/// opened is crossed rather than mistaken for the enclosing one. A `}` met while none of the
/// skip's own braces is open ends it whatever its depth: see
/// [`crate::lossless::recover::unexpected`].
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
/// `Tilde`, `DotDot`, `Caret` and `RAngle` are in; `PathSeparator`, `LAngle`, `FatArrow`,
/// `Asterisk`, `Plus` and `Minus` are out. A root's sigil starts a rooted path, which is a value and
/// a type head, so a skip that ran past one would fold a whole rooted name into the junk before it.
/// A `>` closes a generic list — the same argument the other three closers make. The six that stay
/// out begin no production this suite can restart at: `::` and `=>` only ever join or separate,
/// `<` only ever *follows* a path, and `*`/`+`/`-` reach the tree through recovery alone. Stopping
/// at one of those would only split one `Error` node into two.
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
      | Kind::Tilde
      | Kind::DotDot
      | Kind::Caret
      | Kind::Spread
      | Kind::RBracket
      | Kind::RBrace
      | Kind::RParen
      | Kind::RAngle
  )
}

/// Where a document loop is willing to restart after an entry failed: the head of another entry.
///
/// **Fifteen spellings against GraphQL's thirteen** — `use` and `mod` are the extra two, and the
/// absence of either would cost every such entry after a broken definition. `extend` is in the set
/// for the reason GraphQL's is: an extension is an entry even though it is not a definition. The
/// set is shared by the three roots, so the executable root restarts at a `mod` too, and reports
/// it there.
#[inline]
fn is_definition_start(kind: Kind, keyword: Option<Keyword>) -> bool {
  match kind {
    Kind::InlineString | Kind::BlockString => true,
    Kind::Identifier => matches!(
      keyword,
      Some(
        Keyword::Query
          | Keyword::Mutation
          | Keyword::Subscription
          | Keyword::Fragment
          | Keyword::Schema
          | Keyword::Directive
          | Keyword::Scalar
          | Keyword::Type
          | Keyword::Interface
          | Keyword::Union
          | Keyword::Enum
          | Keyword::Input
          | Keyword::Extend
          | Keyword::Use
          | Keyword::Mod
      )
    ),
    _ => false,
  }
}

/// GraphQLx's **four** delimiter pairs, for the recovery scans' depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because a scan asks it one thing:
/// whether it is `b'{'`. Both scans count every pair in one depth, blind to which closer answers
/// which opener; [`crate::lossless::recover::unexpected`] also counts the braces it opens, and
/// never takes a `}` it did not open.
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

/// Whether `keyword` opens an item of a module's body: a `use` item, a module, a type-system
/// definition or an extension.
///
/// The SDL root's entries, which is what a body holds. An executable definition's keyword is not
/// among them, so a query or a fragment in a body is junk between two items.
#[inline]
pub(crate) const fn starts_mod_item(keyword: Option<Keyword>) -> bool {
  matches!(
    keyword,
    Some(
      Keyword::Use
        | Keyword::Mod
        | Keyword::Extend
        | Keyword::Schema
        | Keyword::Directive
        | Keyword::Scalar
        | Keyword::Type
        | Keyword::Interface
        | Keyword::Union
        | Keyword::Enum
        | Keyword::Input
    )
  )
}

/// Where a module's body is willing to restart after an item failed or junk stood between two:
/// the head of an item — a description's string, or a keyword of [`starts_mod_item`] — or a `}`.
///
/// The `}` is in the set so that the scan stops **before** the body's closer instead of running
/// past it. Whether a `}` at hand *is* the body's closer is not this predicate's to say: the body
/// decides it by the lexer's count of open braces, and
/// [`resync_to_closing`](crate::lossless::recover::resync_to_closing) keeps a `}` that closes
/// wreckage from ever being asked.
#[inline]
fn is_mod_restart_point(kind: Kind, keyword: Option<Keyword>) -> bool {
  match kind {
    Kind::InlineString | Kind::BlockString | Kind::RBrace => true,
    Kind::Identifier => starts_mod_item(keyword),
    _ => false,
  }
}

/// The one pair a module's body is delimited by, for the scan that resynchronises inside one.
///
/// **Braces alone, where [`delimiters`] has four pairs.** A body follows the braces as written,
/// by the lexer's own count of them, and the scan has to agree with that count token for token.
/// With the other three pairs neutral, an unclosed `(`, `[` or `<` in the wreckage hides no item
/// head from the predicate and no `)`, `]` or `>` can close a brace.
#[inline]
fn braces(kind: &Kind) -> Balance<u8> {
  match kind {
    Kind::LBrace => Balance::Open(b'{'),
    Kind::RBrace => Balance::Close(b'{'),
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

  /// A brace-delimited value — an object, a `set!{ … }` or a `map!{ … }` — ran to end of input
  /// before its `}` arrived.
  fn unclosed_object<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedBrace::<SimpleSpan, GraphQLx>::brace_of(open))
  }

  /// A parenthesised list — an argument list or a variables definition — ran to end of input
  /// before its `)` arrived.
  fn unclosed_parens<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    crate::lossless::recover::unclosed(inp, UnclosedParen::<SimpleSpan, GraphQLx>::paren_of(open))
  }

  /// A generic list ran to end of input before its `>` arrived.
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
    // #168's scan allowance. Read here rather than inside the substrate helper because the
    // committed side is `smear-lexer`'s token tally and `crate::lossless::recover` may not name
    // that crate — `lossless_isolation.rs` enforces it. **Both numbers are produce-events**, and
    // the byte count that reads so naturally in this position is the unsound version: one comment
    // token carries arbitrarily many bytes, so bytes grow without the meter growing and the guard
    // stays open. See `scan_allowance_exhausted`.
    let exhausted = crate::lossless::recover::scan_allowance_exhausted(
      inp.token_budget().spent(),
      inp.state().token().tokens(),
    );
    crate::lossless::recover::unexpected(
      inp,
      expected,
      delimiters,
      |t| is_sync_point(kind_of(t)),
      K::Error.raw(),
      exhausted,
    )
  }

  /// A document entry returned `Err`: skip its wreckage and stop **before** the next entry's head.
  ///
  /// [`crate::lossless::recover::resync_to`], which reports nothing — the failed entry's own
  /// `expect` already emitted at the position the failure happened, and a second diagnostic here
  /// would point at whatever the resync happens to start on.
  fn resync_to_definition<'inp, Src, Ctx>(inp) {
    // #168's scan allowance. Read here rather than inside the substrate helper because the
    // committed side is `smear-lexer`'s token tally and `crate::lossless::recover` may not name
    // that crate — `lossless_isolation.rs` enforces it. **Both numbers are produce-events**, and
    // the byte count that reads so naturally in this position is the unsound version: one comment
    // token carries arbitrarily many bytes, so bytes grow without the meter growing and the guard
    // stays open. See `scan_allowance_exhausted`.
    let exhausted = crate::lossless::recover::scan_allowance_exhausted(
      inp.token_budget().spent(),
      inp.state().token().tokens(),
    );
    crate::lossless::recover::resync_to(
      inp,
      delimiters,
      |t| is_definition_start(kind_of(t), keyword_of(t)),
      K::Error.raw(),
      exhausted,
    )
  }

  /// An item of a module's body returned `Err`: skip its wreckage and stop **before** the body's
  /// next restart point — the head of an item, or the `}` that ends the body.
  ///
  /// [`crate::lossless::recover::resync_to_closing`], which reports nothing, with the one pair a
  /// body is delimited by. `floor` is how many braces were open once the body's own `{` was read.
  /// The braces open above it now are the ones the wreckage opened and did not close, and the
  /// scan spends that many `}` on the wreckage before anything can restart the body.
  ///
  /// # What the skip does not promise
  ///
  /// Which `}` ends the body is the brace count's to say, in the body itself, and never this
  /// skip's: so nothing below can put an item in the wrong module, and what it costs is
  /// diagnostics.
  ///
  /// - **A skip that does not scan.** Under an exhausted scan allowance, or after a scan that ran
  ///   to the end of the text without finding a restart point, one token is taken into an
  ///   `Error` node and the body's next turn starts from the token after it. An item keyword
  ///   inside the wreckage can then start an item, and the item stands in the module the braces
  ///   say it does.
  /// - **A string inside parentheses.** The scan balances braces alone, so a string between a
  ///   `(` and its `)` is a restart point like any other: the next turn reads it as a
  ///   description, and the rest of the parenthesised run is junk of its own.
  fn resync_to_mod_item<'inp, Src, Ctx>(inp, floor: usize) {
    // One read of the lexer's state serves both numbers, and it is read here rather than inside
    // the substrate helper for the reason `resync_to_definition` gives: the state is
    // `smear-lexer`'s, and `crate::lossless::recover` may not name that crate. It is the state at
    // the committed position, so a head that is only peeked is in neither number.
    let state: super::LexerState = inp.state();
    let exhausted: bool = crate::lossless::recover::scan_allowance_exhausted(
      inp.token_budget().spent(),
      state.token().tokens(),
    );
    crate::lossless::recover::resync_to_closing(
      inp,
      braces,
      |t| is_mod_restart_point(kind_of(t), keyword_of(t)),
      state.brace_depth().saturating_sub(floor),
      K::Error.raw(),
      exhausted,
    )
  }

  /// Something that starts no item stands between two items of a module's body. **One report**,
  /// at the run's first token, and then [`resync_to_mod_item`] skips to the body's next restart
  /// point.
  ///
  /// When the head of an item or the body's `}` follows the run and the scan allowance is open,
  /// the whole run costs that one diagnostic and one `Error` node, where a report for each token
  /// would cost one of each per token. A `}` that closes a brace the junk opened is part of the
  /// run: `floor` is what tells it from the body's own. Otherwise the skip takes one token and the
  /// next turn reports again, so the run costs a report per token: [`resync_to_mod_item`] says
  /// when that happens, and why it misplaces nothing.
  fn skip_mod_junk<'inp, Src, Ctx>(inp, floor: usize) {
    crate::lossless::recover::report_unexpected(inp, MOD_ITEM_HEADS)?;
    resync_to_mod_item::<Src, Ctx>(inp, floor)
  }

  /// One token, whatever it is, into an `Error` node, and nothing at the end of input.
  ///
  /// What a loop does when a turn of it has left the input where it found it, so that its
  /// termination rests on the position and not on what any production happens to consume.
  fn skip_one<'inp, Src, Ctx>(inp) {
    let mark = inp.cst_mark();
    if inp.try_expect_or_stop(|_| true)?.is_some() {
      inp.cst_start_at(mark, K::Error.raw());
      inp.cst_finish(K::Error.raw());
    }
    Ok(())
  }
}
