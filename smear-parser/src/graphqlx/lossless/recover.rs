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

use smear_lexer::graphqlx::lossless::{LosslessToken, LosslessTokenKind as Kind};
use tokora::{
  SimpleSpan,
  cache::{Cache as _, PeekedTokenExt as _},
  error::{UnclosedAngle, UnclosedBrace, UnclosedBracket, UnclosedParen},
  input::Balance,
  span::Spanned,
  utils::{
    DowncastRef,
    typenum::{U2, U3, U4, U5},
  },
};

use crate::graphqlx::{GraphQLx, kinds::SyntaxKind as K};

use super::{
  GraphqlxLosslessLexer, GraphqlxLosslessToken, Keyword,
  trivia::{kind_of, peek_as, peek_kind},
};
use crate::lossless::recover::keyword_of;

/// The token kinds a `Value` may begin with — what an "expected a value" diagnostic names.
///
/// `true`, `false`, `null`, `set` and `map` are `Identifier`s to the lexer, so none of them needs
/// an entry of its own; the value dispatcher separates them on their spelling, not on their kind.
///
/// **`Tilde`, `DotDot`, `Caret` and `Char` are here and are not in GraphQL's set.** A root's sigil
/// opens a rooted enum value (`^::ns::Colour`), and a `char` literal is a value of its own, two
/// value heads this dialect has and the other does not. `PathSeparator` is not: a `::` only joins,
/// so no value starts with one.
pub(crate) const VALUE_HEADS: &[Kind] = &[
  Kind::Dollar,
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::Char,
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
  Kind::Char,
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

/// The two brackets a typed form opens with after its type path: `{` for a typed object and `(`
/// for a typed value. What a schema value reports as expected where type arguments are followed by
/// neither.
pub(crate) const TYPED_FORM_HEADS: &[Kind] = &[Kind::LBrace, Kind::LParen];

/// The `}` a `const { … }` block closes with: what its body reports as expected where something
/// ended the expression without being the block's own, a `,` among them. A set, a map, an object
/// and a typed object inside a block report the same where something ended a member without being
/// their own.
pub(crate) const BRACE_CLOSER_HEADS: &[Kind] = &[Kind::RBrace];

/// What may begin an operand inside a `const { … }` block: the scalars' tokens, a prefixed string
/// and a `char` literal, a name, a root's sigil, the `(` of a parenthesis, the `[` of a list and
/// the `{` of an object. A number carries no sign there, so `-` is no head; and no variable is, as
/// in a constant position.
pub(crate) const BLOCK_OPERAND_HEADS: &[Kind] = &[
  Kind::Int,
  Kind::Float,
  Kind::InlineString,
  Kind::BlockString,
  Kind::Char,
  Kind::PrefixedString,
  Kind::Identifier,
  Kind::Tilde,
  Kind::DotDot,
  Kind::Caret,
  Kind::LParen,
  Kind::LBracket,
  Kind::LBrace,
];

/// What may follow an operand inside a `const { … }` block and go on with the expression: one of
/// the ten operators, or a `,`. A report through this set renders as
/// [`OperatorOrSeparator`](crate::graphqlx::error::Expectation::OperatorOrSeparator), which no other
/// set of this module has the contents of.
pub(crate) const OPERATOR_OR_SEPARATOR_HEADS: &[Kind] = &[
  Kind::Asterisk,
  Kind::Slash,
  Kind::Percent,
  Kind::Plus,
  Kind::Minus,
  Kind::Shl,
  Kind::Shr,
  Kind::Ampersand,
  Kind::Caret,
  Kind::Pipe,
  Kind::Separator,
];

/// The two tokens a `const { … }` block opens with, `const` and `{`: what a schema position offers
/// where its value would be an expression. A report through this set renders as
/// [`ConstBlock`](crate::graphqlx::error::Expectation::ConstBlock), which no other set of this module
/// has the contents of.
pub(crate) const CONST_BLOCK_HEADS: &[Kind] = &[Kind::Identifier, Kind::LBrace];

/// The `:` between an object field's name and its value: what a field inside a block reports where
/// the `:` is missing, since the field reads it rather than expects it. A report through this set
/// renders as [`Colon`](crate::graphqlx::error::Expectation::Colon), as a declined `expect` of the
/// `:` does, and no other set of this module has the contents of.
pub(crate) const COLON_HEADS: &[Kind] = &[Kind::Colon];

/// The `=>` between a map entry's key and its value, reported as [`COLON_HEADS`] is, and rendering
/// as [`FatArrow`](crate::graphqlx::error::Expectation::FatArrow).
pub(crate) const FAT_ARROW_HEADS: &[Kind] = &[Kind::FatArrow];

/// The `{` a set's or a map's members open with after its `!`: what one inside a block reports
/// where the `{` is missing, since the form reads it rather than expects it. A report through this
/// set renders as [`LBrace`](crate::graphqlx::error::Expectation::LBrace), as a declined `expect` of
/// the `{` does.
///
/// The `{` is named twice so that no other set of this module has these contents. A set is read by
/// what it holds, and three others hold the `{` alone and render with no expectation: a module's
/// body, a schema's operation types and a definition's fields, whose messages stay as they are.
pub(crate) const COLLECTION_BODY_HEADS: &[Kind] = &[Kind::LBrace, Kind::LBrace];

/// What the productions around one inside a `const { … }` block read next, as far out as the
/// nearest production delimited by braces: a set over `,`, `=>`, `)`, `]`, `>` and `:`.
///
/// Inside a block every token is an operand's, a delimiter, or junk, and each delimiter is the
/// next read of exactly one production. A brace-delimited production — the block, an object, a
/// typed object, a set and a map — ends at its own `}` or at the end of input, so nothing outside
/// it reads past it, and the set its inside is handed starts again: [`Owners::MEMBERS`], the `,`
/// its members are separated by, with the `=>` beside it while a map entry's key is read. Every
/// other production adds what it reads itself: a parenthesis and a typed value their `)`, a list
/// its `]` and its `,`, type arguments their `>`, and a list type its `]`. A field reading on to
/// its `:` adds that `:`. A production that is not brace-delimited leaves a token of the set it was
/// handed to the production that reads it, and reports its own closer missing in front of it; the
/// innermost production that reads a token takes it, so a list reads its own `,` and `]` before it
/// asks the set.
///
/// `Copy` and passed by value, one byte, so handing it down costs what the `&'static [Kind]` it
/// replaces did, which held the caller's delimiter and no one else's.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub(crate) struct Owners(u8);

impl Owners {
  /// What the expression a block holds is handed: nothing, since the block's `}` ends it and it has
  /// no members.
  pub(crate) const NONE: Self = Self(0);

  /// What a member of an object, a typed object, a set or a map is handed: the `,` between two.
  pub(crate) const MEMBERS: Self = Self(Self::bit(Kind::Separator));

  /// The one bit `kind` has in the set, or none for a kind no production around another reads.
  const fn bit(kind: Kind) -> u8 {
    match kind {
      Kind::Separator => 1,
      Kind::FatArrow => 1 << 1,
      Kind::RParen => 1 << 2,
      Kind::RBracket => 1 << 3,
      Kind::RAngle => 1 << 4,
      Kind::Colon => 1 << 5,
      _ => 0,
    }
  }

  /// This set and `kind`, which the production handing the set down reads next.
  #[inline]
  pub(crate) const fn with(self, kind: Kind) -> Self {
    Self(self.0 | Self::bit(kind))
  }

  /// Whether a production around the one holding this set reads `kind` next.
  #[inline]
  pub(crate) const fn owns(self, kind: Kind) -> bool {
    self.0 & Self::bit(kind) != 0
  }
}

/// Whether `kind` begins an operand inside a `const { … }` block: [`BLOCK_OPERAND_HEADS`], and the
/// `$` of a variable, which a block reports and still builds.
#[inline]
pub(crate) const fn begins_block_operand(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Dollar
      | Kind::Int
      | Kind::Float
      | Kind::InlineString
      | Kind::BlockString
      | Kind::Char
      | Kind::PrefixedString
      | Kind::Identifier
      | Kind::Tilde
      | Kind::DotDot
      | Kind::Caret
      | Kind::LParen
      | Kind::LBracket
      | Kind::LBrace
  )
}

/// Whether `kind` is one of the nine tokens an expression's operator is written with outside a
/// block: `+`, `-`, `*`, `/`, `%`, `<<`, `&`, `|` and `!`.
#[inline]
pub(crate) const fn is_operator(kind: Kind) -> bool {
  matches!(
    kind,
    Kind::Plus
      | Kind::Minus
      | Kind::Asterisk
      | Kind::Slash
      | Kind::Percent
      | Kind::Shl
      | Kind::Ampersand
      | Kind::Pipe
      | Kind::Bang
  )
}

/// Whether `kind`, directly after a value where no second value may follow, can only go on with an
/// expression: an operator's token, a `>`, which closes nothing there, a `^`, or a `(`. A number
/// whose token begins with `-` goes on with one too, which its kind cannot say: see
/// [`super::trivia::peek_signed_number`].
#[inline]
pub(crate) const fn continues_an_expression(kind: Kind) -> bool {
  is_operator(kind) || matches!(kind, Kind::RAngle | Kind::Caret | Kind::LParen)
}

/// Whether `kind` is a number's, whose token may begin with `-`.
#[inline]
pub(crate) const fn is_number(kind: Kind) -> bool {
  matches!(kind, Kind::Int | Kind::Float)
}

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
      | Kind::Char
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
/// opened is crossed rather than mistaken for the enclosing one. No hole
/// [`crate::lossless::recover::unexpected`] keeps holds a `}` met while none of the skip's own
/// braces was open: see its docs.
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
/// `Tilde`, `DotDot`, `Caret`, `Char` and `RAngle` are in, and so are a block's `Separator` and
/// prefixed string, which only a lexer in a block's mode makes, so no skip outside a block changes
/// for them; `PathSeparator`, `LAngle`, `FatArrow`,
/// `Asterisk`, `Plus` and `Minus` are out. A root's sigil starts a rooted path, which is a value and
/// a type head, so a skip that ran past one would fold a whole rooted name into the junk before it.
/// A `char` literal starts a value, as a number does. A `>` closes a generic list — the same
/// argument the other three closers make. The six that stay out begin no production this suite can
/// restart at: `::` and `=>` only ever join or separate, `<` only ever *follows* a path, and
/// `*`/`+`/`-` reach the tree through recovery alone. Stopping at one of those would only split one
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
      | Kind::Char
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
      | Kind::Separator
      | Kind::PrefixedString
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
    Kind::Identifier => restarts_a_root(keyword),
    _ => false,
  }
}

/// Whether `keyword` is one of the fifteen spellings [`is_definition_start`] restarts a root at.
///
/// One table for the two: a list on a header ends in front of no keyword a root would not restart
/// at.
#[inline]
const fn restarts_a_root(keyword: Option<Keyword>) -> bool {
  matches!(
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
  )
}

/// GraphQLx's **four** delimiter pairs, for the recovery scans' depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because a scan asks it one thing:
/// whether it is `b'{'`. Both scans count every pair in one depth, blind to which closer answers
/// which opener; [`crate::lossless::recover::unexpected`] also watches the braces it opens, and
/// keeps no hole holding a `}` it did not open.
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
// A list on a definition's header, and what it ends in front of.
//
// A list between parentheses stands in a member where a member's production calls it, and on a
// header where a definition's own production does. A member's list ends at the `}` of the braces it
// stands in. A header's has no such `}`, since no brace of its definition is open, so one that has
// lost its `)` read the definition's body as members and the next definition's keyword as a
// member's name, and lost both definitions at the `:` it then missed: smear issue #295. The
// header's entries, `directive.rs`'s `header_directives` and `definition.rs`'s
// `header_arguments_definition`, take their owner as one of the markers below. Such a list ends,
// unclosed, in front of what its owner reads next and in front of the start of a definition, where
// a test below finds that to be what stands at its head, and it reads as before everywhere else. A
// member's entries are not touched.
//
// A head a valid list can hold, a name or a description's string, is decided through the peek
// window, read no further than the member there would read and no further than the cache retains,
// and a stop reports the `:` that member would miss, the token the window found. Where a window
// meets the end of input across a run of lexer errors, the parse lexes that run a second time,
// until tokora retains the end of input after an error run. A head no valid list holds is
// reported first, as `unexpected` reports it, and decided behind that report by a look-ahead that
// is given back. So is a directive definition's location spelled like a keyword a root restarts
// at, which no valid location is.
//
// A look-ahead finds its evidence only where every token it crosses on the way passes one step,
// `cross`: no name a root restarts at with anything but a `:` behind it, no `{` but the owner's
// body's, no word the owner's list stops at directly inside a directive's arguments, and no
// bracket that closes out of order or nests deeper than the parse has left. Behind their own
// report, main's skip and root rebuild a definition from any part of the text that nobody asked,
// so the step asks every part.
// ---------------------------------------------------------------------------------------------

/// Whether `keyword` names what an `extend` extends: the eight `extension.rs`'s
/// `type_system_extension` dispatches on.
#[inline]
const fn is_extendable(keyword: Option<Keyword>) -> bool {
  matches!(
    keyword,
    Some(
      Keyword::Schema
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

/// Whether a definition starts at a name spelled `keyword` that a token of `kind`, spelled
/// `spelled`, stands behind: the name is one a root restarts at, and the token behind it is
/// neither a `:`, which makes the name a member's, nor a second such keyword, at which a root
/// would restart instead. `extend` and the keyword of what it extends are one start.
#[inline]
const fn starts_with(keyword: Option<Keyword>, kind: Kind, spelled: Option<Keyword>) -> bool {
  if !restarts_a_root(keyword) {
    return false;
  }
  match kind {
    Kind::Colon => false,
    Kind::Identifier if restarts_a_root(spelled) => {
      matches!(keyword, Some(Keyword::Extend)) && is_extendable(spelled)
    }
    _ => true,
  }
}

/// Whether `where`, `repeatable` or `on` is the word its owner reads next where a token of `kind`,
/// spelled `spelled`, stands behind it: [`starts_with`]'s test of what stands behind, a `:` making
/// the word a member's name and a keyword a root restarts at the start of a definition.
#[inline]
const fn word_goes_on(kind: Kind, spelled: Option<Keyword>) -> bool {
  match kind {
    Kind::Colon => false,
    Kind::Identifier => !restarts_a_root(spelled),
    _ => true,
  }
}

/// Whether a name spelled `keyword`, which a look-ahead crosses on its way to its evidence, may be
/// the keyword of a definition that begins there, `behind` the kind of the token behind it: the
/// keyword is one a root restarts at, and anything but a `:` stands behind it, the end of input
/// included. Main's skip and root, behind their own report, build a definition at such a name, a
/// bare one where nothing names it, so what follows it is no evidence for the owner. Every name
/// crossed is asked so, at every depth: a directive's, a member's, a step of a path, and any
/// inside a directive's arguments, a path's type arguments or a body. Behind a `:` the name is a
/// member's: a definition main builds from a keyword with a `:` or the end of input behind it is
/// not counted lost, and the end of input refuses all the same, which only removes stops.
#[inline]
const fn may_start(keyword: Option<Keyword>, behind: Option<Kind>) -> bool {
  restarts_a_root(keyword) && !matches!(behind, Some(Kind::Colon))
}

/// How many brackets a look-ahead's crossing holds open at most. A crossing that would nest
/// deeper finds no evidence, which only removes stops; the bound keeps the crossing's state in a
/// fixed array.
const CROSS_CAP: usize = 32;

/// The brackets a look-ahead's crossing holds open, innermost last, each as the closer that
/// answers it: one stack over every pair this dialect has, `()`, `[]` and `<>`, with the opener
/// of what it crosses at its bottom: a body's own `{`, or a group's own `(` or `<`.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Crossing {
  closers: [Kind; CROSS_CAP],
  depth: usize,
}

impl Crossing {
  /// A crossing that holds nothing open.
  const fn new() -> Self {
    Self {
      closers: [Kind::RParen; CROSS_CAP],
      depth: 0,
    }
  }

  /// How many brackets it holds open.
  const fn depth(&self) -> usize {
    self.depth
  }

  /// The closer that answers the innermost bracket it holds open, where it holds one.
  const fn innermost(&self) -> Option<Kind> {
    if self.depth == 0 {
      None
    } else {
      Some(self.closers[self.depth - 1])
    }
  }

  /// Holds open a bracket that `closer` answers: `false` where [`CROSS_CAP`] are open already.
  const fn enter(&mut self, closer: Kind) -> bool {
    if self.depth == CROSS_CAP {
      return false;
    }
    self.closers[self.depth] = closer;
    self.depth += 1;
    true
  }

  /// Closes the innermost bracket it holds open.
  const fn leave(&mut self) {
    self.depth = self.depth.saturating_sub(1);
  }
}

/// Whether the reading that would take a look-ahead's evidence has room for `stack` frames, the
/// brackets the crossing holds open where it asks, the one at hand among them, at a parser depth
/// of `depth` under a ceiling of `limitation`, with two frames kept back. Both numbers are the
/// parser's own, `InputRef::recursion`'s, which every production that reads a bracket descends
/// against; the lexer's tally of open brackets is no measure of them, since a closer that answers
/// nothing lowers it while the frames stay held.
#[inline]
const fn fits(depth: usize, limitation: usize, stack: usize) -> bool {
  stack.saturating_add(2) <= limitation.saturating_sub(depth)
}

/// How many tokens a look-ahead takes at most, trivia among them, from where it starts. One that
/// would take more finds no evidence, and the list reads as main reads it. The bound keeps what a
/// look-ahead lexes, and gives back to be lexed again, to a constant at each head it is made at,
/// however long a run of trivia or of crossed tokens stands there. A run of lexer errors is no
/// token: tokora lexes it whole inside the one read that meets it and hands the parser none of
/// it, so the cap does not count it, and a look-ahead lexes such a run in full where it reaches
/// one.
const LOOK_AHEAD_CAP: usize = 256;

/// What a look-ahead may still take of [`LOOK_AHEAD_CAP`], and whether it has met the cap, behind
/// which nothing it reads is evidence.
#[derive(Debug, Clone, Copy)]
pub(crate) struct Reach {
  left: usize,
  spent: bool,
}

impl Reach {
  /// The whole of [`LOOK_AHEAD_CAP`].
  const fn new() -> Self {
    Self {
      left: LOOK_AHEAD_CAP,
      spent: false,
    }
  }

  /// Whether the look-ahead has met the cap.
  const fn spent(&self) -> bool {
    self.spent
  }

  /// One token more: `false` where the cap is met, which spends the reach.
  const fn take(&mut self) -> bool {
    if self.left == 0 {
      self.spent = true;
      return false;
    }
    self.left -= 1;
    true
  }
}

/// The owner of a list on a definition's header, as a type: what the owner's production reads
/// behind the list, in front of which the list ends.
///
/// A marker has no size, so a call passes nothing for one, and each constant is folded into the
/// loop compiled for its owner. Every marker states all four.
pub(crate) trait Header: Copy {
  /// A body, `{ … }`, behind the owner's directives: an object, an interface, an input object, an
  /// enum, a schema, and the extension of each.
  const BODY: bool;
  /// A union's members, `= …`: a union and its extension.
  const MEMBERS: bool;
  /// A `where` clause behind the owner's directives: a scalar, an object, an interface, an input
  /// object, a union, and the extensions of the last four.
  const WHERE: bool;
  /// `repeatable` and `on`: a directive definition, behind its own list and behind its
  /// directives.
  const TAIL: bool;
}

/// A scalar definition's header: a `where` clause.
#[derive(Debug, Clone, Copy)]
pub(crate) struct ScalarHeader;

impl Header for ScalarHeader {
  const BODY: bool = false;
  const MEMBERS: bool = false;
  const WHERE: bool = true;
  const TAIL: bool = false;
}

/// An object's, an interface's and an input object's header, and their extensions': a `where`
/// clause, then a body.
#[derive(Debug, Clone, Copy)]
pub(crate) struct FieldsHeader;

impl Header for FieldsHeader {
  const BODY: bool = true;
  const MEMBERS: bool = false;
  const WHERE: bool = true;
  const TAIL: bool = false;
}

/// An enum's and a schema's header, and their extensions': a body.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BodyHeader;

impl Header for BodyHeader {
  const BODY: bool = true;
  const MEMBERS: bool = false;
  const WHERE: bool = false;
  const TAIL: bool = false;
}

/// A union's header and its extension's: the members, then a `where` clause.
#[derive(Debug, Clone, Copy)]
pub(crate) struct UnionHeader;

impl Header for UnionHeader {
  const BODY: bool = false;
  const MEMBERS: bool = true;
  const WHERE: bool = true;
  const TAIL: bool = false;
}

/// A directive definition's header, behind its own list and behind its directives: `repeatable`,
/// then `on`.
#[derive(Debug, Clone, Copy)]
pub(crate) struct DirectiveHeader;

impl Header for DirectiveHeader {
  const BODY: bool = false;
  const MEMBERS: bool = false;
  const WHERE: bool = false;
  const TAIL: bool = true;
}

/// A scalar extension's header and a directive extension's: nothing behind the directives.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BareHeader;

impl Header for BareHeader {
  const BODY: bool = false;
  const MEMBERS: bool = false;
  const WHERE: bool = false;
  const TAIL: bool = false;
}

/// Whether a list on `owner`'s header asks at a head of `kind` that no valid list holds: at a `{`
/// where the owner reads a body, at an `=` where it reads a union's members, and at each other
/// such head its caller hands it.
#[inline]
pub(crate) fn asks<O: Header>(_: O, kind: Kind) -> bool {
  match kind {
    Kind::LBrace => O::BODY,
    Kind::Equal => O::MEMBERS,
    _ => true,
  }
}

/// Whether `keyword` is a word `owner` reads next: `where` where the owner reads one, and
/// `repeatable` and `on` on a directive definition.
#[inline]
fn ends_at_word<O: Header>(_: O, keyword: Option<Keyword>) -> bool {
  match keyword {
    Some(Keyword::Where) => O::WHERE,
    Some(Keyword::Repeatable | Keyword::On) => O::TAIL,
    _ => false,
  }
}

/// One token of the peek window, as a header's tests read it.
#[derive(Debug, Clone)]
pub(crate) enum Seen<T> {
  /// Not read: the window would be wider than the cache retains, or it came back short of it,
  /// where the input ends in front of it or the scanner stopped.
  Short,
  /// A trivia token.
  Trivia,
  /// A significant token: its kind, the keyword it spells where it spells one, and the token
  /// itself with its span.
  Token(Kind, Option<Keyword>, Spanned<T, SimpleSpan>),
}

/// What a header's tests read of `entry`, the window's token and its span where it holds one.
#[inline]
fn seen<S>(entry: Option<(&LosslessToken<S>, &SimpleSpan)>) -> Seen<LosslessToken<S>>
where
  LosslessToken<S>: Clone + DowncastRef<Keyword>,
{
  match entry {
    None => Seen::Short,
    Some((token, _)) if token.is_trivia() => Seen::Trivia,
    Some((token, span)) => Seen::Token(
      token.kind(),
      keyword_of(token),
      Spanned::new(*span, Clone::clone(token)),
    ),
  }
}

/// The first token that is no trivia behind a name or a description's string, where a header's
/// test decides on it.
#[derive(Debug, Clone)]
pub(crate) struct Behind<T> {
  /// Where it stands in the peek window, the head at 0.
  at: usize,
  /// Its kind.
  kind: Kind,
  /// The keyword it spells, where it spells one.
  spelled: Option<Keyword>,
  /// The token and its span, as the window read them: what a stop reports in place of the `:`.
  found: Spanned<T, SimpleSpan>,
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
  ///
  /// Inside a `const { … }` block, where the lexer's state says it is, this is a guard that no
  /// text reaches: every production there recovers through `value.rs`'s `block_junk`, which reads
  /// a bracket through the production that owns its closer. Here it reports and takes one token,
  /// so the shared scan, whose one depth over every pair reads past what a block's productions
  /// read next, never runs in the block's mode.
  fn unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    if inp.state().in_const_block() {
      crate::lossless::recover::report_unexpected(inp, expected)?;
      return skip_one::<Src, Ctx>(inp);
    }
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

// ---------------------------------------------------------------------------------------------
// What a list on a definition's header reads at its head before it ends there. The window reads
// are for a head a valid list can hold and make no report but the stop's own; the look-ahead is for
// a head no valid list holds, is made behind that head's report, and is given back. A directive
// definition's location spelled like a definition's keyword is decided the same way.
//
// Every token a look-ahead crosses on its way to its evidence passes `cross`. A reader takes no
// token itself but the structure it reads, the head, an `@`, an `=`, a `|`, a path's sigils and
// `::`, the owner's body's `{` and `}` and a group's own opener and closer, and its terminal
// evidence. Every token it takes, trivia among them, it takes through `kind_ahead` and
// `take_ahead`, within `LOOK_AHEAD_CAP`.
// ---------------------------------------------------------------------------------------------

lossless_production! {
  dialect = graphqlx::lossless;

  /// The token at `at` of the peek window, the head at 0, read through the narrowest window that
  /// holds it, and only where the active cache retains that window: [`Seen::Short`] where it does
  /// not, and where the window comes back short of the token at the end of input.
  ///
  /// A window the cache retains lexes nothing the parse does not lex next, but for one shape: where
  /// it meets the end of input across a run of lexer errors, tokora retains nothing of the run
  /// behind the newest token it holds, and the parse lexes the run a second time, each error
  /// charged to the produce-event budget twice. That stays so until tokora retains the end of
  /// input after an error run. The read raises a stop the scanner meets while it fills, and that
  /// stop reads as [`Seen::Short`] too: it is no answer, and the parse's own read meets it where it
  /// stands.
  fn window_at<'inp, Src, Ctx>(
    inp,
    at: usize,
    out: &mut Seen<GraphqlxLosslessToken<'inp, Src>>,
  ) {
    let cache = inp.cache();
    if at >= cache.len().saturating_add(cache.remaining()) {
      *out = Seen::Short;
      return Ok(());
    }
    let read = match at {
      1 => inp.peek_map::<U2, _, _>(|window| {
        seen(window.get(1).map(|entry| (entry.token(), entry.span())))
      }),
      2 => inp.peek_map::<U3, _, _>(|window| {
        seen(window.get(2).map(|entry| (entry.token(), entry.span())))
      }),
      3 => inp.peek_map::<U4, _, _>(|window| {
        seen(window.get(3).map(|entry| (entry.token(), entry.span())))
      }),
      4 => inp.peek_map::<U5, _, _>(|window| {
        seen(window.get(4).map(|entry| (entry.token(), entry.span())))
      }),
      _ => Ok(Seen::Short),
    };
    *out = match read {
      Ok(token) => token,
      Err(stop) if tokora::error::MaybeTerminal::is_terminal(&stop) => Seen::Short,
      Err(error) => return Err(error),
    };
    Ok(())
  }

  /// The first token behind the peek window's `at` that is no trivia, where at most one trivia
  /// token stands between, each read as [`window_at`] reads it. `None` where a window is not read
  /// or comes back short of it, or where two trivia tokens stand between, since each blank is a
  /// token of its own and the window sees past one.
  fn token_behind<'inp, Src, Ctx>(
    inp,
    at: usize,
    out: &mut Option<Behind<GraphqlxLosslessToken<'inp, Src>>>,
  ) {
    let mut behind = at + 1;
    let mut token = Seen::Short;
    window_at::<Src, Ctx>(inp, behind, &mut token)?;
    if matches!(token, Seen::Trivia) {
      behind += 1;
      window_at::<Src, Ctx>(inp, behind, &mut token)?;
    }
    *out = match token {
      Seen::Token(kind, spelled, found) => Some(Behind {
        at: behind,
        kind,
        spelled,
        found,
      }),
      Seen::Short | Seen::Trivia => None,
    };
    Ok(())
  }

  /// The report a member's `expect` makes where `found` stands in place of its `:`: the same
  /// code, the same span and the same token found, built from what the window that made the stop
  /// read. Nothing is read again, so no budget spent since can leave the stop without its report.
  fn colon_missing<'inp, Src, Ctx>(
    inp,
    found: Spanned<GraphqlxLosslessToken<'inp, Src>, SimpleSpan>,
  ) {
    inp.emit_error(crate::lossless::trivia::expected_at::<
      GraphqlxLosslessLexer<'inp, Src>,
      Ctx,
      GraphQLx,
    >(found, Kind::Colon))?;
    Ok(())
  }

  /// At a name at a member position of a list on `owner`'s header: whether the list ends in front
  /// of it, which it does at the start of a definition and at `where`, `repeatable` or `on` where
  /// the owner reads the word, each by what stands behind the name; and where it ends, the report
  /// the member there would make first, the `:` it misses at the token behind the name.
  ///
  /// The spelling is asked first, so a member named like none of those words costs one test of
  /// it. Only behind a spelling that matches are the brace count, which must be what it was behind
  /// the list's `(`, and the peek window read. The window reads no further than that member reads
  /// to its `:`, so every lexer error in front of the report is reported where the member would
  /// report it, and no further than the cache retains: where the cache retains less, no stop is
  /// made and the member reads as it always did. What it lexes twice is a run of lexer errors it
  /// meets the end of input across, as [`window_at`] says.
  ///
  /// What begins at the stop reads on as it reads at a root, and may take in what main reads as a
  /// definition, as a definition or a where clause that begins in the junk does: at a mixed root,
  /// `type T @d(x: 1 type U { f: Int }` builds `U` where main builds the operation `{ f: Int }`,
  /// and `type T @d(x: 1 where K: L { f: Int }` reads `{ f: Int }` as `T`'s body behind its where
  /// clause. Both are among the costs of a stop the repair accepts; no look-ahead decides them.
  fn name_ends_header<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, ends: &mut bool) {
    let keyword = peek_as::<Src, Ctx, Keyword>(inp)?;
    let start = restarts_a_root(keyword);
    if !start && !ends_at_word(owner, keyword) {
      return Ok(());
    }
    if inp.state().brace_depth() != open_at {
      return Ok(());
    }
    let mut behind = None;
    token_behind::<Src, Ctx>(inp, 0, &mut behind)?;
    let Some(behind) = behind else {
      return Ok(());
    };
    let stops = if start {
      starts_with(keyword, behind.kind, behind.spelled)
    } else {
      word_goes_on(behind.kind, behind.spelled)
    };
    if stops {
      colon_missing::<Src, Ctx>(inp, behind.found)?;
      *ends = true;
    }
    Ok(())
  }

  /// At a description's string at a member position of a directive definition's own list: whether
  /// the list ends in front of it, the description of a definition that starts behind it; and
  /// where it ends, the report the member there would make first, the `:` its name misses at the
  /// token behind that name.
  ///
  /// The brace count must be what it was behind the list's `(`. The window is then read in two
  /// stages, each as far as the cache retains it. First the string and up to two tokens behind it;
  /// where the first of them that is no trivia is not a name spelled like a keyword a root restarts
  /// at, nothing more is asked. Where it is, the token behind that name. A token past what the
  /// cache retains is not read, so no stop is made there, and the valid
  /// `directive @d("doc" type: Int)` costs no produce event more than it would without this test.
  /// A run of lexer errors a window meets the end of input across is lexed again, as
  /// [`window_at`] says.
  fn description_ends_header<'inp, Src, Ctx>(inp, open_at: usize, ends: &mut bool) {
    if inp.state().brace_depth() != open_at {
      return Ok(());
    }
    let mut name = None;
    token_behind::<Src, Ctx>(inp, 0, &mut name)?;
    let Some(Behind { at, kind: Kind::Identifier, spelled: keyword, .. }) = name else {
      return Ok(());
    };
    if !restarts_a_root(keyword) {
      return Ok(());
    }
    let mut behind = None;
    token_behind::<Src, Ctx>(inp, at, &mut behind)?;
    let Some(behind) = behind else {
      return Ok(());
    };
    if starts_with(keyword, behind.kind, behind.spelled) {
      colon_missing::<Src, Ctx>(inp, behind.found)?;
      *ends = true;
    }
    Ok(())
  }

  /// Behind the report of a name where a directive definition reads a location, a name whose
  /// spelling is no location's: whether the locations end in front of it, which leaves it to the
  /// item loop. They end only at a name spelled like a keyword a root restarts at, and only where a
  /// look-ahead, given back, takes the name and finds behind it, across any trivia, a token that
  /// starts a definition with it by [`starts_with`]'s test. Everywhere else they go on, and the
  /// name is a location misspelled, as main reads it: at a name spelled like no such keyword, at
  /// the end of input behind the name, where the allowance is spent, and where the look-ahead
  /// meets a stop or an error, or would take more than [`LOOK_AHEAD_CAP`] tokens.
  ///
  /// No valid location is spelled like such a keyword, so no valid document makes the look-ahead.
  /// What it lexes and gives back, at most [`LOOK_AHEAD_CAP`] tokens and any run of lexer errors
  /// among them, the durable produce-event budget sees, and so does the allowance.
  fn location_ends<'inp, Src, Ctx>(inp, keyword: Option<Keyword>, ends: &mut bool) {
    if !restarts_a_root(keyword) {
      return Ok(());
    }
    // `unexpected`'s guard; `scan_allowance_exhausted` says what it is.
    let exhausted = crate::lossless::recover::scan_allowance_exhausted(
      inp.token_budget().spent(),
      inp.state().token().tokens(),
    );
    let mut starts = false;
    if !exhausted {
      // Declined always, so every token the look-ahead reads, every report a lexer makes inside it
      // and any stop it meets are taken back with it.
      let mut reach = Reach::new();
      inp.attempt(|inp| {
        let mut read = false;
        if location_ahead::<Src, Ctx>(inp, keyword, &mut reach, &mut read).is_ok() {
          starts = read && !reach.spent();
        }
        None::<()>
      });
    }
    *ends = starts;
    Ok(())
  }

  /// [`unexpected`] behind its report: [`crate::lossless::recover::skip_unexpected`] with this
  /// dialect's four pairs and its restart predicate, under the allowance `exhausted` that its
  /// caller read where [`unexpected`] reads it. In a `const { … }` block it takes one token, as
  /// [`unexpected`] does there.
  fn skip_unexpected<'inp, Src, Ctx>(inp, exhausted: bool) {
    if inp.state().in_const_block() {
      return skip_one::<Src, Ctx>(inp);
    }
    crate::lossless::recover::skip_unexpected(
      inp,
      delimiters,
      |t| is_sync_point(kind_of(t)),
      K::Error.raw(),
      exhausted,
    )
  }

  /// [`unexpected`] at a head no valid list holds, at a member position of a list on `owner`'s
  /// header in which nothing the list has read leaves a brace open: its report first, then a
  /// look-ahead, given back, for what the owner reads next, and its skip only where the look-ahead
  /// finds nothing. Where it finds something the list ends in front of the head, which the report
  /// has named.
  ///
  /// The report goes first so that nothing the look-ahead spends of a durable budget can stop the
  /// parse in front of it, and the head is reported once whichever way the look-ahead goes. The
  /// allowance is read where [`unexpected`] reads it, in front of the report, and governs the
  /// look-ahead and the skip alike: where it is spent no look-ahead is made, and the skip takes
  /// what [`unexpected`]'s would.
  ///
  /// What the look-ahead costs is what it lexes, which it gives back to be lexed again: at most
  /// [`LOOK_AHEAD_CAP`] tokens, trivia among them, the one it stops at and any run of lexer errors
  /// among them, which the cap cannot count, once at this head. The durable produce-event budget
  /// sees all of it, as the real cost it is, and so does the allowance, which reads that budget.
  fn unexpected_or_ends<'inp, Src, Ctx>(
    inp,
    expected: &'static [Kind],
    open_at: usize,
    owner: impl Header,
    ends: &mut bool,
  ) {
    if inp.state().in_const_block() {
      return unexpected::<Src, Ctx>(inp, expected);
    }
    // `unexpected`'s guard, read where it reads it; `scan_allowance_exhausted` says what it is.
    let exhausted = crate::lossless::recover::scan_allowance_exhausted(
      inp.token_budget().spent(),
      inp.state().token().tokens(),
    );
    crate::lossless::recover::report_unexpected(inp, expected)?;
    if !exhausted {
      // Declined always, so every token the look-ahead reads, every report a lexer makes inside it
      // and any stop it meets are taken back with it. A stop or an error is no evidence, and the
      // skip below then meets it for real.
      let mut found = false;
      let mut reach = Reach::new();
      inp.attempt(|inp| {
        let mut read = false;
        if ahead::<Src, Ctx>(inp, open_at, owner, &mut reach, &mut read).is_ok() {
          found = read && !reach.spent();
        }
        None::<()>
      });
      if found {
        *ends = true;
        return Ok(());
      }
    }
    skip_unexpected::<Src, Ctx>(inp, exhausted)
  }

  /// What a look-ahead reads at a head no valid list holds, on `owner`'s header: whether what the
  /// owner reads next stands there.
  ///
  /// - **A `{`:** the owner's body, read by [`body_ahead`]. The `}` that answers it is written,
  ///   and behind it stands the end of input, a `}` that closes a block around the list, or the
  ///   start of a definition.
  /// - **An `@`:** the owner's next directive. One or more, each an `@`, its path and its
  ///   arguments where a `(` follows, and behind them what may stand behind a body, or what the
  ///   owner reads next by its own test.
  /// - **An `=`:** a union's members. A `|` or none, paths between `|`, and behind them what may
  ///   stand behind a body, or a `where` by its test.
  /// - **A string:** the description of a definition that starts behind it.
  ///
  /// Every token it crosses on the way to what it asks about passes [`cross`], and where that
  /// refuses one it finds nothing. So it finds nothing where a definition may begin in what it
  /// crosses, at any depth, where a `{` but the owner's body's stands there, where a directive's
  /// arguments hold a word the owner's list stops at, and where a bracket closes out of order or
  /// nests deeper than the parse has left. Where it meets the end of input in place of what it
  /// asks about, it finds nothing either.
  ///
  /// Every token it takes, trivia among them, it takes within `reach`, through [`kind_ahead`] and
  /// [`take_ahead`], so it takes no more than [`LOOK_AHEAD_CAP`]: where it would, `reach` is spent
  /// and what it read is no evidence, whatever it would have answered.
  fn ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    owner: impl Header,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    match head {
      Some(Kind::LBrace) => body_ahead::<Src, Ctx>(inp, open_at, owner, true, reach, found),
      Some(Kind::At) => directives_ahead::<Src, Ctx>(inp, open_at, owner, reach, found),
      Some(Kind::Equal) => members_ahead::<Src, Ctx>(inp, open_at, owner, reach, found),
      Some(Kind::InlineString | Kind::BlockString) => {
        take_ahead::<Src, Ctx>(inp, reach)?;
        starts_ahead::<Src, Ctx>(inp, reach, found)
      }
      _ => Ok(()),
    }
  }

  /// The kind of the next token that is no trivia, ahead, the trivia in front of it taken within
  /// `reach`. `None` at the end of input, and where `reach` is spent: where it is already, and
  /// where the trivia would take the look-ahead past [`LOOK_AHEAD_CAP`], which spends it there.
  fn kind_ahead<'inp, Src, Ctx>(inp, reach: &mut Reach, out: &mut Option<Kind>) {
    *out = None;
    if reach.spent() {
      return Ok(());
    }
    inp.skip_while(|token| token.is_trivia() && reach.take())?;
    if !reach.spent() {
      *out = peek_kind::<Src, Ctx>(inp)?;
    }
    Ok(())
  }

  /// One token ahead taken, within `reach`: where none of it is left, nothing is taken and `reach`
  /// is spent.
  fn take_ahead<'inp, Src, Ctx>(inp, reach: &mut Reach) {
    if reach.take() {
      inp.try_expect_or_stop(|_| true)?;
    }
    Ok(())
  }

  /// A body ahead, at its `{`: the `}` that answers it, every token between crossed by [`cross`]
  /// with the body's `{` at the bottom of what the crossing holds open, and behind it what
  /// [`behind_ahead`] finds. So nothing inside may begin a definition, no `{` stands inside, and
  /// every bracket inside closes in order: the body's `}` ends it only where nothing above its `{`
  /// is open. A bracket the body leaves open would hide what stands behind it from main's scan,
  /// which counts the closers blind to which opener each answers, and main's walk would then read
  /// the body.
  ///
  /// `end_counts` says whether the end of input behind the body is evidence. Behind a body reached
  /// through a directive it is not: at a mixed root main's walk reads that `{` as an operation.
  fn body_ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    owner: impl Header,
    end_counts: bool,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    let mut crossing = Crossing::new();
    let mut refused = false;
    opener::<Src, Ctx>(inp, &mut crossing, Kind::RBrace, reach, &mut refused)?;
    while !refused {
      let mut head = None;
      kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
      match head {
        None => return Ok(()),
        Some(Kind::RBrace) if crossing.depth() == 1 => {
          take_ahead::<Src, Ctx>(inp, reach)?;
          return behind_ahead::<Src, Ctx>(inp, open_at, end_counts, reach, found);
        }
        Some(_) => cross::<Src, Ctx>(inp, owner, &mut crossing, false, reach, &mut refused)?,
      }
    }
    Ok(())
  }

  /// A group ahead, at its opener: the opener taken through [`opener`], so it is held open at the
  /// bottom of the crossing and asked for the nesting the parse has left as every bracket inside
  /// it is, every token behind it crossed by [`cross`], and the `closer` that ends the group
  /// taken: a directive's arguments, `(` to `)`, or a path's type arguments, `<` to `>`. With
  /// nothing open inside, only `closer` ends it, and any other closer refuses. `words` asks
  /// [`cross`] for the owner's words where nothing but the group's opener is open, which a
  /// directive's arguments do. `written` says whether the whole group is, with nothing in it
  /// refused.
  fn group_ahead<'inp, Src, Ctx>(
    inp,
    owner: impl Header,
    closer: Kind,
    words: bool,
    reach: &mut Reach,
    written: &mut bool,
  ) {
    let mut crossing = Crossing::new();
    let mut refused = false;
    opener::<Src, Ctx>(inp, &mut crossing, closer, reach, &mut refused)?;
    while !refused {
      let mut head = None;
      kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
      match head {
        None => return Ok(()),
        Some(kind) if kind == closer && crossing.depth() == 1 => {
          take_ahead::<Src, Ctx>(inp, reach)?;
          *written = true;
          return Ok(());
        }
        Some(_) => cross::<Src, Ctx>(inp, owner, &mut crossing, words, reach, &mut refused)?,
      }
    }
    Ok(())
  }

  /// An opener ahead, taken and held open on `crossing`, answered by `closer`: `refused` says
  /// where that would nest past [`CROSS_CAP`], or past what [`fits`] finds the parse has left.
  /// Every production the reading that would take the evidence runs at an opener requests a
  /// frame of the parser there, where main scans the brackets flat, and a refusal at the parse's
  /// nesting ceiling would end the document.
  fn opener<'inp, Src, Ctx>(
    inp,
    crossing: &mut Crossing,
    closer: Kind,
    reach: &mut Reach,
    refused: &mut bool,
  ) {
    let (depth, limitation) = (inp.recursion().depth(), inp.recursion().limitation());
    if crossing.enter(closer) && fits(depth, limitation, crossing.depth()) {
      take_ahead::<Src, Ctx>(inp, reach)?;
    } else {
      *refused = true;
    }
    Ok(())
  }

  /// One token a look-ahead crosses on its way to its evidence, taken, and asked: `refused` says
  /// where nothing behind it can be evidence for `owner`, since main's skip and root, behind their
  /// own report, rebuild a definition from any token at any depth. That is so at
  ///
  /// - a name [`may_start`] finds the keyword of a definition;
  /// - a `{`, since only the owner's body opens one, and that one its reader takes itself;
  /// - where `words` asks it, with nothing but the group's own opener open: a word the owner's
  ///   list stops at, by [`ends_at_word`], with a token behind it [`word_goes_on`] accepts, since
  ///   a directive's arguments end there in the reading that would take the evidence;
  /// - `set` or `map` with a `!` behind it, where [`fits`] finds no room for one frame more than
  ///   the crossing holds: the reading that would take the evidence requests a frame for the
  ///   collection there, in front of the `{` it then asks for;
  /// - a closer that does not answer the innermost bracket `crossing` holds open;
  /// - an opener [`opener`] refuses;
  /// - the end of input.
  ///
  /// Nothing is taken at a `{` or a closer it refuses, nor at the end of input.
  fn cross<'inp, Src, Ctx>(
    inp,
    owner: impl Header,
    crossing: &mut Crossing,
    words: bool,
    reach: &mut Reach,
    refused: &mut bool,
  ) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    match head {
      None | Some(Kind::LBrace) => *refused = true,
      Some(Kind::Identifier) => {
        let keyword = peek_as::<Src, Ctx, Keyword>(inp)?;
        take_ahead::<Src, Ctx>(inp, reach)?;
        let word = words && crossing.depth() == 1 && ends_at_word(owner, keyword);
        let collection = matches!(keyword, Some(Keyword::Set | Keyword::Map));
        if restarts_a_root(keyword) || word || collection {
          let mut behind = None;
          kind_ahead::<Src, Ctx>(inp, reach, &mut behind)?;
          let word_ends = match behind {
            Some(kind) if word => word_goes_on(kind, peek_as::<Src, Ctx, Keyword>(inp)?),
            _ => false,
          };
          let (depth, limitation) = (inp.recursion().depth(), inp.recursion().limitation());
          let frame = collection
            && behind == Some(Kind::Bang)
            && !fits(depth, limitation, crossing.depth().saturating_add(1));
          *refused = may_start(keyword, behind) || word_ends || frame;
        }
      }
      Some(Kind::LParen) => opener::<Src, Ctx>(inp, crossing, Kind::RParen, reach, refused)?,
      Some(Kind::LBracket) => opener::<Src, Ctx>(inp, crossing, Kind::RBracket, reach, refused)?,
      Some(Kind::LAngle) => opener::<Src, Ctx>(inp, crossing, Kind::RAngle, reach, refused)?,
      Some(closer @ (Kind::RParen | Kind::RBracket | Kind::RAngle | Kind::RBrace)) => {
        if crossing.innermost() == Some(closer) {
          crossing.leave();
          take_ahead::<Src, Ctx>(inp, reach)?;
        } else {
          *refused = true;
        }
      }
      Some(_) => take_ahead::<Src, Ctx>(inp, reach)?,
    }
    Ok(())
  }

  /// What may stand behind a body, the owner's directives or a union's members, ahead: the end of
  /// input where `end_counts` says so, a `}` that closes a block around the list, by the brace
  /// count at that `}`, or the start of a definition, a description's string in front of it or
  /// not.
  fn behind_ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    end_counts: bool,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    match head {
      None => *found = end_counts,
      Some(Kind::RBrace) => *found = closes_enclosing_block(open_at, inp.state().brace_depth()),
      Some(Kind::InlineString | Kind::BlockString) => {
        take_ahead::<Src, Ctx>(inp, reach)?;
        starts_ahead::<Src, Ctx>(inp, reach, found)?;
      }
      Some(Kind::Identifier) => starts_ahead::<Src, Ctx>(inp, reach, found)?,
      Some(_) => {}
    }
    Ok(())
  }

  /// The start of a definition ahead, at a name: [`starts_with`]'s test of it and of the token
  /// behind it, read across any trivia. The end of input behind the name is no evidence.
  fn starts_ahead<'inp, Src, Ctx>(inp, reach: &mut Reach, found: &mut bool) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    if head != Some(Kind::Identifier) {
      return Ok(());
    }
    let keyword = peek_as::<Src, Ctx, Keyword>(inp)?;
    if !restarts_a_root(keyword) {
      return Ok(());
    }
    take_ahead::<Src, Ctx>(inp, reach)?;
    let mut behind = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut behind)?;
    if let Some(kind) = behind {
      *found = starts_with(keyword, kind, peek_as::<Src, Ctx, Keyword>(inp)?);
    }
    Ok(())
  }

  /// A location ahead, at a name spelled `keyword`: the name taken, and whether the token behind
  /// it, read across any trivia, starts a definition with it by [`starts_with`]'s test. The end of
  /// input behind it starts none.
  fn location_ahead<'inp, Src, Ctx>(
    inp,
    keyword: Option<Keyword>,
    reach: &mut Reach,
    starts: &mut bool,
  ) {
    take_ahead::<Src, Ctx>(inp, reach)?;
    let mut behind = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut behind)?;
    if let Some(kind) = behind {
      *starts = starts_with(keyword, kind, peek_as::<Src, Ctx, Keyword>(inp)?);
    }
    Ok(())
  }

  /// The owner's next directive ahead, at its `@`: each `@`, its path, read by [`path_ahead`],
  /// and where a `(` follows the path, its arguments, read by [`group_ahead`] with the owner's
  /// words asked, again while an `@` follows; and behind them what [`next_ahead`] finds. Where a
  /// path or arguments are unwritten, or crossed into a refusal, nothing is found.
  fn directives_ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    owner: impl Header,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    loop {
      let mut head = None;
      kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
      if head != Some(Kind::At) {
        break;
      }
      take_ahead::<Src, Ctx>(inp, reach)?;
      let mut written = false;
      path_ahead::<Src, Ctx>(inp, owner, reach, &mut written)?;
      if written {
        let mut next = None;
        kind_ahead::<Src, Ctx>(inp, reach, &mut next)?;
        if next == Some(Kind::LParen) {
          written = false;
          group_ahead::<Src, Ctx>(inp, owner, Kind::RParen, true, reach, &mut written)?;
        }
      }
      if !written {
        return Ok(());
      }
    }
    next_ahead::<Src, Ctx>(inp, open_at, owner, reach, found)
  }

  /// What may stand behind the owner's directives, ahead: what [`behind_ahead`] finds, or what the
  /// owner reads next by its own test, a body behind a `{` where it reads one, with no end of
  /// input behind that body as evidence, a union's members behind an `=` where it reads them, and
  /// `where`, `repeatable` or `on` where it reads the word.
  fn next_ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    owner: impl Header,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    let word = head == Some(Kind::Identifier)
      && ends_at_word(owner, peek_as::<Src, Ctx, Keyword>(inp)?);
    match head {
      Some(Kind::LBrace) if asks(owner, Kind::LBrace) => {
        body_ahead::<Src, Ctx>(inp, open_at, owner, false, reach, found)
      }
      Some(Kind::Equal) if asks(owner, Kind::Equal) => {
        members_ahead::<Src, Ctx>(inp, open_at, owner, reach, found)
      }
      Some(Kind::Identifier) if word => {
        take_ahead::<Src, Ctx>(inp, reach)?;
        word_ahead::<Src, Ctx>(inp, reach, found)
      }
      _ => behind_ahead::<Src, Ctx>(inp, open_at, true, reach, found),
    }
  }

  /// A union's members ahead, at its `=`: a `|` or none, then paths with a `|` between each two,
  /// each read by [`path_ahead`]; and behind them what [`behind_ahead`] finds, or a `where` by its
  /// test where the owner reads one. Where a path is unwritten, or crossed into a refusal, nothing
  /// is found.
  fn members_ahead<'inp, Src, Ctx>(
    inp,
    open_at: usize,
    owner: impl Header,
    reach: &mut Reach,
    found: &mut bool,
  ) {
    take_ahead::<Src, Ctx>(inp, reach)?;
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    if head == Some(Kind::Pipe) {
      take_ahead::<Src, Ctx>(inp, reach)?;
    }
    loop {
      let mut written = false;
      path_ahead::<Src, Ctx>(inp, owner, reach, &mut written)?;
      if !written {
        return Ok(());
      }
      let mut next = None;
      kind_ahead::<Src, Ctx>(inp, reach, &mut next)?;
      if next != Some(Kind::Pipe) {
        break;
      }
      take_ahead::<Src, Ctx>(inp, reach)?;
    }
    let mut tail = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut tail)?;
    if tail == Some(Kind::Identifier) && ends_at_word(owner, peek_as::<Src, Ctx, Keyword>(inp)?) {
      take_ahead::<Src, Ctx>(inp, reach)?;
      return word_ahead::<Src, Ctx>(inp, reach, found);
    }
    behind_ahead::<Src, Ctx>(inp, open_at, true, reach, found)
  }

  /// The word its owner reads next, ahead, the word taken: [`word_goes_on`]'s test of the token
  /// behind it, read across any trivia. The end of input behind the word is no evidence.
  fn word_ahead<'inp, Src, Ctx>(inp, reach: &mut Reach, found: &mut bool) {
    let mut behind = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut behind)?;
    if let Some(kind) = behind {
      *found = word_goes_on(kind, peek_as::<Src, Ctx, Keyword>(inp)?);
    }
    Ok(())
  }

  /// A path ahead, as a directive's name and a union's member are written, which `ty.rs`'s
  /// `type_path` reads: a root, `~ ::` or `^ ::` once or `.. ::` any number of times, or none; a
  /// name, and `:: Name` any number of times, each name crossed by [`cross`]; and where a `<`
  /// follows, its type arguments, read by [`group_ahead`]. `written` says whether the whole of it
  /// is, with nothing in it refused.
  fn path_ahead<'inp, Src, Ctx>(inp, owner: impl Header, reach: &mut Reach, written: &mut bool) {
    let mut head = None;
    kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
    match head {
      Some(Kind::Tilde | Kind::Caret) => {
        take_ahead::<Src, Ctx>(inp, reach)?;
        kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
        if head != Some(Kind::PathSeparator) {
          return Ok(());
        }
        take_ahead::<Src, Ctx>(inp, reach)?;
      }
      _ => {
        while head == Some(Kind::DotDot) {
          take_ahead::<Src, Ctx>(inp, reach)?;
          kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
          if head != Some(Kind::PathSeparator) {
            return Ok(());
          }
          take_ahead::<Src, Ctx>(inp, reach)?;
          kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
        }
      }
    }
    let mut crossing = Crossing::new();
    let mut refused = false;
    loop {
      kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
      if head != Some(Kind::Identifier) {
        return Ok(());
      }
      cross::<Src, Ctx>(inp, owner, &mut crossing, false, reach, &mut refused)?;
      if refused {
        return Ok(());
      }
      kind_ahead::<Src, Ctx>(inp, reach, &mut head)?;
      if head != Some(Kind::PathSeparator) {
        break;
      }
      take_ahead::<Src, Ctx>(inp, reach)?;
    }
    if head == Some(Kind::LAngle) {
      return group_ahead::<Src, Ctx>(inp, owner, Kind::RAngle, false, reach, written);
    }
    *written = true;
    Ok(())
  }
}
