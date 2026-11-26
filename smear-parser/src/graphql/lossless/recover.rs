//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute,
//! an `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so
//! text fidelity holds regardless.
//!
//! # What is here and what is in the substrate
//!
//! Everything below is **table**: the head sets an "expected one of" diagnostic names,
//! this dialect's three balanced pairs, its restart predicate and its definition-start
//! predicate. The *logic* — report, skip, attribute, guarantee progress — is
//! [`crate::lossless::recover`]'s, shared with every dialect, and its module docs carry the
//! reasoning about `sync_balanced`'s two no-progress cases and the termination rule that follows
//! from them.
//!
//! The wrappers at the bottom exist so a production still writes
//! `unexpected::<Src, Ctx>(inp, VALUE_HEADS)` rather than a five-argument call naming this
//! dialect's tables at every one of ~40 sites. They add no behaviour, exactly as `trivia.rs`'s do.

use smear_lexer::graphql::{
  ContextualKeyword,
  lossless::{LosslessToken, LosslessTokenKind as Kind},
};
use tokora::{
  SimpleSpan,
  cache::{Cache as _, PeekedTokenExt as _},
  error::{UnclosedBrace, UnclosedBracket, UnclosedParen},
  input::Balance,
  span::Spanned,
  utils::{
    DowncastRef,
    typenum::{U2, U3, U4, U5},
  },
};

use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

use super::{
  GraphqlLosslessLexer, GraphqlLosslessToken,
  trivia::{kind_of, peek_as, peek_kind},
};

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
/// token-kind expectation set onto
/// [`Expectation::Unspecified`](crate::graphql::error::Expectation::Unspecified) anyway, so a
/// second set naming the keyword would report the same sentence. The precise
/// "expected the keyword `on`" wording needs the dialect error rather than tokora's
/// token-kind one, and no production here carries the bound that would reach it.
pub(crate) const TYPE_CONDITION_HEADS: &[Kind] = &[Kind::Identifier];

/// The token kinds a `VariableDefinition` may begin with: its `$`, or the leading string of a
/// description — `VariableDefinition : Description? Variable : Type …`, so both string kinds
/// are heads here and the `$` is only the most common one.
pub(crate) const VARIABLE_DEFINITION_HEADS: &[Kind] =
  &[Kind::InlineString, Kind::BlockString, Kind::Dollar];

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

/// The token kinds that may follow a description on a definition: the keyword that opens the
/// described definition, and nothing else.
///
/// Every alternative that carries a `Description?` opens with a keyword — an `Identifier` to
/// the lexer. The one that does not is the shorthand `OperationDefinition : SelectionSet`, so
/// this set is [`EXECUTABLE_DEFINITION_HEADS`] minus the two string kinds a description cannot
/// follow itself with, and minus the `{` that is the whole point of the exclusion.
pub(crate) const DESCRIBED_DEFINITION_HEADS: &[Kind] = &[Kind::Identifier];

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
/// The last two are *spelling* rules — `DirectiveLocation` admits twenty names and
/// `FragmentName` is `Name but not "on"` — which this kind-level set cannot express and does not
/// try to: `lossless/mod.rs`'s `expectation_of` collapses every token-kind expectation onto
/// [`Expectation::Unspecified`](crate::graphql::error::Expectation::Unspecified) anyway, so a finer
/// set here would report the same sentence. The precise wording needs the dialect error rather than
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

/// The token kinds that may open a `DirectiveExtension`'s tail. Like a scalar extension, it has
/// exactly one shape — `extend directive @ Name Directives` — so its directives are mandatory.
pub(crate) const DIRECTIVE_EXTENSION_TAIL_HEADS: &[Kind] = &[Kind::At];

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

/// The `)` a parenthesised list closes with: what such a list reports as expected where it
/// stops short of one, at a `}` that belongs to a block around it. See
/// [`closes_enclosing_block`].
pub(crate) const PARENS_CLOSER_HEADS: &[Kind] = &[Kind::RParen];

/// The `]` a list type or a list value closes with, reported as [`PARENS_CLOSER_HEADS`] is.
pub(crate) const LIST_CLOSER_HEADS: &[Kind] = &[Kind::RBracket];

pub(crate) use crate::lossless::recover::opener_span;

/// Whether `kind` closes one of GraphQL's three delimiter pairs.
///
/// Asked where the grammar requires exactly one component — a type after a `:`, a value after
/// an `=` — and the head starts none. A closer there is the closer of the block the position
/// stands in, so it is reported and left for that block's own loop, where every other head is
/// junk to consume. Consuming it would cost the block its end and everything after the block
/// with it.
#[inline]
pub(crate) const fn is_closer(kind: Kind) -> bool {
  matches!(kind, Kind::RParen | Kind::RBracket | Kind::RBrace)
}

/// Whether a `}` at the head of a list delimited by `(` or `[` belongs to a block around the
/// list, and so is not the list's to consume.
///
/// `open_at` is the lexer's count of open braces right after the list's own opener was read,
/// and `now` is the count at the committed position: the `}` at the head is only peeked, so it
/// is not in `now` yet. The `}` closes a brace that was open before the list began exactly when
/// one was (`open_at > 0`) and the list's own content has left none of its own open
/// (`now <= open_at`). The list then reports its own closer as missing and returns, and the
/// block's loop finds its `}` where it stands. Taking it as junk would cost the block its end,
/// and hand the list everything after the block.
///
/// In the two other cases the `}` stays junk of the list. With no brace open before the list it
/// closes nothing and is a stray; with more open now than then it closes a brace the list's own
/// content opened. A `)` or `]` that is not the list's own is junk too, as it has always been:
/// no count tells a stray one from an enclosing one.
#[inline]
pub(crate) const fn closes_enclosing_block(open_at: usize, now: usize) -> bool {
  open_at > 0 && now <= open_at
}

/// Where a recovery is willing to stop: a token that could start something the caller knows
/// how to parse, or a closer the enclosing shape knows how to consume.
///
/// The closers are in the set even though nothing starts with one — stopping *before* `]` is
/// what lets an enclosing `list_value` close on its own delimiter instead of running to end of
/// input. The skip stops at this only where its depth over every pair is zero, so a `]` inside
/// nesting it opened is crossed rather than mistaken for the enclosing one. No hole
/// [`crate::lossless::recover::unexpected`] keeps holds a `}` met while none of the skip's own
/// braces was open: see its docs.
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
    Kind::Identifier => restarts_a_root(keyword),
    _ => false,
  }
}

/// Whether `keyword` is one of the thirteen spellings [`is_definition_start`] restarts a root at.
///
/// One table for the two: a list on a header ends in front of no keyword a root would not restart
/// at.
#[inline]
const fn restarts_a_root(keyword: Option<ContextualKeyword>) -> bool {
  matches!(
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
  )
}

/// [`crate::lossless::recover::keyword_of`] with this dialect's keyword projection pinned.
///
/// The wrapper exists so the two predicates below read as membership tests over a named enum
/// rather than as turbofished downcasts.
#[inline]
fn keyword_of<T: DowncastRef<ContextualKeyword>>(token: &T) -> Option<ContextualKeyword> {
  crate::lossless::recover::keyword_of(token)
}

/// GraphQL's three delimiter pairs, for the recovery scans' depth counting.
///
/// The pair identity is a `u8` rather than a bespoke enum because a scan asks it one thing:
/// whether it is `b'{'`. Both scans count every pair in one depth, blind to which closer answers
/// which opener; [`crate::lossless::recover::unexpected`] also watches the braces it opens, and
/// keeps no hole holding a `}` it did not open.
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
// A list on a definition's header, and what it ends in front of.
//
// GraphQLx's twin, with this dialect's tables: its thirteen restart keywords, its extension
// dispatch, no `where` and no module. A list between parentheses stands in a member where a
// member's production calls it, and on a header where a definition's own production does. A
// header's has no `}` to end at, since no brace of its definition is open, so one that has lost
// its `)` read the definition's body as members and the next definition's keyword as a member's
// name, and lost both definitions at the `:` it then missed: smear issue #295. The header's
// entries, `directive.rs`'s `header_directives` and `definition.rs`'s
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

/// Whether `keyword` names what an `extend` extends: the eight `document.rs`'s
/// `type_system_extension` dispatches on.
#[inline]
const fn is_extendable(keyword: Option<ContextualKeyword>) -> bool {
  matches!(
    keyword,
    Some(
      ContextualKeyword::Schema
        | ContextualKeyword::Directive
        | ContextualKeyword::Scalar
        | ContextualKeyword::Type
        | ContextualKeyword::Interface
        | ContextualKeyword::Union
        | ContextualKeyword::Enum
        | ContextualKeyword::Input
    )
  )
}

/// Whether a definition starts at a name spelled `keyword` that a token of `kind`, spelled
/// `spelled`, stands behind: the name is one a root restarts at, and the token behind it is
/// neither a `:`, which makes the name a member's, nor a second such keyword, at which a root
/// would restart instead. `extend` and the keyword of what it extends are one start.
#[inline]
const fn starts_with(
  keyword: Option<ContextualKeyword>,
  kind: Kind,
  spelled: Option<ContextualKeyword>,
) -> bool {
  if !restarts_a_root(keyword) {
    return false;
  }
  match kind {
    Kind::Colon => false,
    Kind::Identifier if restarts_a_root(spelled) => {
      matches!(keyword, Some(ContextualKeyword::Extend)) && is_extendable(spelled)
    }
    _ => true,
  }
}

/// Whether `repeatable` or `on` is the word its owner reads next where a token of `kind`, spelled
/// `spelled`, stands behind it: [`starts_with`]'s test of what stands behind, a `:` making the
/// word a member's name and a keyword a root restarts at the start of a definition.
#[inline]
const fn word_goes_on(kind: Kind, spelled: Option<ContextualKeyword>) -> bool {
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
/// crossed is asked so, at every depth: a directive's, a member's, and any inside a directive's
/// arguments or a body. Behind a `:` the name is a member's: a definition main builds from a
/// keyword with a `:` or the end of input behind it is not counted lost, and the end of input
/// refuses all the same, which only removes stops.
#[inline]
const fn may_start(keyword: Option<ContextualKeyword>, behind: Option<Kind>) -> bool {
  restarts_a_root(keyword) && !matches!(behind, Some(Kind::Colon))
}

/// How many brackets a look-ahead's crossing holds open at most. A crossing that would nest
/// deeper finds no evidence, which only removes stops; the bound keeps the crossing's state in a
/// fixed array.
const CROSS_CAP: usize = 32;

/// The brackets a look-ahead's crossing holds open, innermost last, each as the closer that
/// answers it: one stack over both pairs this dialect has, `()` and `[]`, and a body's own `{` at
/// its bottom where the crossing is a body's.
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

/// The owner of a list on a definition's header, as a type: what the owner's production reads
/// behind the list, in front of which the list ends.
///
/// A marker has no size, so a call passes nothing for one, and each constant is folded into the
/// loop compiled for its owner. Every marker states all three.
pub(crate) trait Header: Copy {
  /// A body, `{ … }`, behind the owner's directives: an object, an interface, an input object, an
  /// enum, a schema, and the extension of each.
  const BODY: bool;
  /// A union's members, `= …`: a union and its extension.
  const MEMBERS: bool;
  /// `repeatable` and `on`: a directive definition, behind its own list and behind its
  /// directives.
  const TAIL: bool;
}

/// An object's, an interface's, an input object's, an enum's and a schema's header, and their
/// extensions': a body.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BodyHeader;

impl Header for BodyHeader {
  const BODY: bool = true;
  const MEMBERS: bool = false;
  const TAIL: bool = false;
}

/// A union's header and its extension's: the members.
#[derive(Debug, Clone, Copy)]
pub(crate) struct UnionHeader;

impl Header for UnionHeader {
  const BODY: bool = false;
  const MEMBERS: bool = true;
  const TAIL: bool = false;
}

/// A directive definition's header, behind its own list and behind its directives: `repeatable`,
/// then `on`.
#[derive(Debug, Clone, Copy)]
pub(crate) struct DirectiveHeader;

impl Header for DirectiveHeader {
  const BODY: bool = false;
  const MEMBERS: bool = false;
  const TAIL: bool = true;
}

/// A scalar's header, a scalar extension's and a directive extension's: nothing behind the
/// directives.
#[derive(Debug, Clone, Copy)]
pub(crate) struct BareHeader;

impl Header for BareHeader {
  const BODY: bool = false;
  const MEMBERS: bool = false;
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

/// Whether `keyword` is a word `owner` reads next: `repeatable` and `on` on a directive
/// definition.
#[inline]
fn ends_at_word<O: Header>(_: O, keyword: Option<ContextualKeyword>) -> bool {
  match keyword {
    Some(ContextualKeyword::Repeatable | ContextualKeyword::On) => O::TAIL,
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
  Token(Kind, Option<ContextualKeyword>, Spanned<T, SimpleSpan>),
}

/// What a header's tests read of `entry`, the window's token and its span where it holds one.
#[inline]
fn seen<S>(entry: Option<(&LosslessToken<S>, &SimpleSpan)>) -> Seen<LosslessToken<S>>
where
  LosslessToken<S>: Clone + DowncastRef<ContextualKeyword>,
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
  spelled: Option<ContextualKeyword>,
  /// The token and its span, as the window read them: what a stop reports in place of the `:`.
  found: Spanned<T, SimpleSpan>,
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
  /// [`crate::lossless::recover::unexpected`] with this dialect's pairs and its restart predicate.
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

  /// A definition returned `Err`: skip its wreckage and stop before the next definition head.
  /// [`crate::lossless::recover::resync_to`] with this dialect's definition-start predicate,
  /// which is deliberately narrower than [`is_sync_point`] — see both predicates' docs.
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
}

// ---------------------------------------------------------------------------------------------
// What a list on a definition's header reads at its head before it ends there. The window reads
// are for a head a valid list can hold and make no report but the stop's own; the look-ahead is for
// a head no valid list holds, is made behind that head's report, and is given back. A directive
// definition's location spelled like a definition's keyword is decided the same way.
//
// Every token a look-ahead crosses on its way to its evidence passes `cross`. A reader takes no
// token itself but the structure it reads, the head, an `@`, an `=`, a `|`, the owner's body's
// `{` and `}` and a directive's own `(` and `)`, and its terminal evidence.
// ---------------------------------------------------------------------------------------------

lossless_production! {
  dialect = graphql::lossless;

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
    out: &mut Seen<GraphqlLosslessToken<'inp, Src>>,
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
    out: &mut Option<Behind<GraphqlLosslessToken<'inp, Src>>>,
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
    found: Spanned<GraphqlLosslessToken<'inp, Src>, SimpleSpan>,
  ) {
    inp.emit_error(crate::lossless::trivia::expected_at::<
      GraphqlLosslessLexer<'inp, Src>,
      Ctx,
      GraphQL,
    >(found, Kind::Colon))?;
    Ok(())
  }

  /// At a name at a member position of a list on `owner`'s header: whether the list ends in front
  /// of it, which it does at the start of a definition and at `repeatable` or `on` on a directive
  /// definition, each by what stands behind the name; and where it ends, the report the member
  /// there would make first, the `:` it misses at the token behind the name.
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
  /// definition, as a definition that begins in the junk does: at a mixed root,
  /// `type T @d(x: 1 type U { f: Int }` builds `U` where main builds the operation `{ f: Int }`.
  /// It is among the costs of a stop the repair accepts; no look-ahead decides it.
  fn name_ends_header<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, ends: &mut bool) {
    let keyword = peek_as::<Src, Ctx, ContextualKeyword>(inp)?;
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
  /// meets a stop or an error.
  ///
  /// No valid location is spelled like such a keyword, so no valid document makes the look-ahead.
  fn location_ends<'inp, Src, Ctx>(
    inp,
    keyword: Option<ContextualKeyword>,
    ends: &mut bool,
  ) {
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
      inp.attempt(|inp| {
        let mut read = false;
        if location_ahead::<Src, Ctx>(inp, keyword, &mut read).is_ok() {
          starts = read;
        }
        None::<()>
      });
    }
    *ends = starts;
    Ok(())
  }

  /// [`unexpected`] behind its report: [`crate::lossless::recover::skip_unexpected`] with this
  /// dialect's pairs and its restart predicate, under the allowance `exhausted` that its caller
  /// read where [`unexpected`] reads it.
  fn skip_unexpected<'inp, Src, Ctx>(inp, exhausted: bool) {
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
  fn unexpected_or_ends<'inp, Src, Ctx>(
    inp,
    expected: &'static [Kind],
    open_at: usize,
    owner: impl Header,
    ends: &mut bool,
  ) {
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
      inp.attempt(|inp| {
        let mut read = false;
        if ahead::<Src, Ctx>(inp, open_at, owner, &mut read).is_ok() {
          found = read;
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
  /// - **An `@`:** the owner's next directive. One or more, each an `@`, its name and its
  ///   arguments where a `(` follows, and behind them what may stand behind a body, or what the
  ///   owner reads next by its own test.
  /// - **An `=`:** a union's members. A `|` or none, names between `|`, and behind them what may
  ///   stand behind a body.
  /// - **A string:** the description of a definition that starts behind it.
  ///
  /// Every token it crosses on the way to what it asks about passes [`cross`], and where that
  /// refuses one it finds nothing. So it finds nothing where a definition may begin in what it
  /// crosses, at any depth, where a `{` but the owner's body's stands there, where a directive's
  /// arguments hold a word the owner's list stops at, and where a bracket closes out of order or
  /// nests deeper than the parse has left. Where it meets the end of input in place of what it
  /// asks about, it finds nothing either.
  fn ahead<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, found: &mut bool) {
    match peek_kind::<Src, Ctx>(inp)? {
      Some(Kind::LBrace) => body_ahead::<Src, Ctx>(inp, open_at, owner, true, found),
      Some(Kind::At) => directives_ahead::<Src, Ctx>(inp, open_at, owner, found),
      Some(Kind::Equal) => members_ahead::<Src, Ctx>(inp, open_at, owner, found),
      Some(Kind::InlineString | Kind::BlockString) => {
        inp.try_expect_or_stop(|_| true)?;
        starts_ahead::<Src, Ctx>(inp, found)
      }
      _ => Ok(()),
    }
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
    found: &mut bool,
  ) {
    let mut crossing = Crossing::new();
    let mut refused = false;
    opener::<Src, Ctx>(inp, &mut crossing, Kind::RBrace, &mut refused)?;
    while !refused {
      match peek_kind::<Src, Ctx>(inp)? {
        None => return Ok(()),
        Some(Kind::RBrace) if crossing.depth() == 1 => {
          inp.try_expect_or_stop(|_| true)?;
          return behind_ahead::<Src, Ctx>(inp, open_at, end_counts, found);
        }
        Some(_) => cross::<Src, Ctx>(inp, owner, &mut crossing, false, &mut refused)?,
      }
    }
    Ok(())
  }

  /// A directive's arguments ahead, at their `(`: the `(` and the `)` that ends them taken, and
  /// every token between crossed by [`cross`], with the owner's words asked at their depth 0.
  /// With nothing open inside, only the `)` ends them, and any other closer refuses. `written`
  /// says whether the whole of them is, with nothing in them refused.
  fn group_ahead<'inp, Src, Ctx>(inp, owner: impl Header, written: &mut bool) {
    inp.try_expect_or_stop(|_| true)?;
    let mut crossing = Crossing::new();
    let mut refused = false;
    while !refused {
      match peek_kind::<Src, Ctx>(inp)? {
        None => return Ok(()),
        Some(Kind::RParen) if crossing.depth() == 0 => {
          inp.try_expect_or_stop(|_| true)?;
          *written = true;
          return Ok(());
        }
        Some(_) => cross::<Src, Ctx>(inp, owner, &mut crossing, true, &mut refused)?,
      }
    }
    Ok(())
  }

  /// An opener ahead, taken and held open on `crossing`, answered by `closer`: `refused` says
  /// where that would nest past [`CROSS_CAP`], or past the nesting the parse has left, two frames
  /// kept back for the reading that would take the evidence. That reading descends a frame for
  /// each bracket the crossing holds, where main scans them flat, and a refusal at the parse's
  /// nesting ceiling would end the document.
  fn opener<'inp, Src, Ctx>(inp, crossing: &mut Crossing, closer: Kind, refused: &mut bool) {
    let left = inp
      .recursion()
      .limitation()
      .saturating_sub(inp.recursion().depth());
    if crossing.enter(closer) && crossing.depth().saturating_add(2) <= left {
      inp.try_expect_or_stop(|_| true)?;
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
  /// - where `words` asks it, at depth 0 of a directive's arguments: a word the owner's list stops
  ///   at, by [`ends_at_word`], with a token behind it [`word_goes_on`] accepts, since those
  ///   arguments end there in the reading that would take the evidence;
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
    refused: &mut bool,
  ) {
    match peek_kind::<Src, Ctx>(inp)? {
      None | Some(Kind::LBrace) => *refused = true,
      Some(Kind::Identifier) => {
        let keyword = peek_as::<Src, Ctx, ContextualKeyword>(inp)?;
        inp.try_expect_or_stop(|_| true)?;
        let word = words && crossing.depth() == 0 && ends_at_word(owner, keyword);
        if restarts_a_root(keyword) || word {
          let behind = peek_kind::<Src, Ctx>(inp)?;
          let word_ends = match behind {
            Some(kind) if word => {
              word_goes_on(kind, peek_as::<Src, Ctx, ContextualKeyword>(inp)?)
            }
            _ => false,
          };
          *refused = may_start(keyword, behind) || word_ends;
        }
      }
      Some(Kind::LParen) => opener::<Src, Ctx>(inp, crossing, Kind::RParen, refused)?,
      Some(Kind::LBracket) => opener::<Src, Ctx>(inp, crossing, Kind::RBracket, refused)?,
      Some(closer @ (Kind::RParen | Kind::RBracket | Kind::RBrace)) => {
        if crossing.innermost() == Some(closer) {
          crossing.leave();
          inp.try_expect_or_stop(|_| true)?;
        } else {
          *refused = true;
        }
      }
      Some(_) => {
        inp.try_expect_or_stop(|_| true)?;
      }
    }
    Ok(())
  }

  /// What may stand behind a body, the owner's directives or a union's members, ahead: the end of
  /// input where `end_counts` says so, a `}` that closes a block around the list, by the brace
  /// count at that `}`, or the start of a definition, a description's string in front of it or
  /// not.
  fn behind_ahead<'inp, Src, Ctx>(inp, open_at: usize, end_counts: bool, found: &mut bool) {
    match peek_kind::<Src, Ctx>(inp)? {
      None => *found = end_counts,
      Some(Kind::RBrace) => *found = closes_enclosing_block(open_at, inp.state().brace_depth()),
      Some(Kind::InlineString | Kind::BlockString) => {
        inp.try_expect_or_stop(|_| true)?;
        starts_ahead::<Src, Ctx>(inp, found)?;
      }
      Some(Kind::Identifier) => starts_ahead::<Src, Ctx>(inp, found)?,
      Some(_) => {}
    }
    Ok(())
  }

  /// The start of a definition ahead, at a name: [`starts_with`]'s test of it and of the token
  /// behind it, read across any trivia. The end of input behind the name is no evidence.
  fn starts_ahead<'inp, Src, Ctx>(inp, found: &mut bool) {
    let keyword = peek_as::<Src, Ctx, ContextualKeyword>(inp)?;
    if !restarts_a_root(keyword) {
      return Ok(());
    }
    inp.try_expect_or_stop(|_| true)?;
    if let Some(kind) = peek_kind::<Src, Ctx>(inp)? {
      *found = starts_with(keyword, kind, peek_as::<Src, Ctx, ContextualKeyword>(inp)?);
    }
    Ok(())
  }

  /// A location ahead, at a name spelled `keyword`: the name taken, and whether the token behind
  /// it, read across any trivia, starts a definition with it by [`starts_with`]'s test. The end of
  /// input behind it starts none.
  fn location_ahead<'inp, Src, Ctx>(
    inp,
    keyword: Option<ContextualKeyword>,
    starts: &mut bool,
  ) {
    inp.try_expect_or_stop(|_| true)?;
    if let Some(kind) = peek_kind::<Src, Ctx>(inp)? {
      *starts = starts_with(keyword, kind, peek_as::<Src, Ctx, ContextualKeyword>(inp)?);
    }
    Ok(())
  }

  /// The owner's next directive ahead, at its `@`: each `@`, its name, crossed by [`cross`], and
  /// where a `(` follows the name, its arguments, read by [`group_ahead`], again while an `@`
  /// follows; and behind them what [`next_ahead`] finds. Where a name is missing or refused, or
  /// arguments are unwritten, nothing is found.
  fn directives_ahead<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, found: &mut bool) {
    while peek_kind::<Src, Ctx>(inp)? == Some(Kind::At) {
      inp.try_expect_or_stop(|_| true)?;
      let mut written = false;
      name_crossed::<Src, Ctx>(inp, owner, &mut written)?;
      if written && peek_kind::<Src, Ctx>(inp)? == Some(Kind::LParen) {
        written = false;
        group_ahead::<Src, Ctx>(inp, owner, &mut written)?;
      }
      if !written {
        return Ok(());
      }
    }
    next_ahead::<Src, Ctx>(inp, open_at, owner, found)
  }

  /// What may stand behind the owner's directives, ahead: what [`behind_ahead`] finds, or what the
  /// owner reads next by its own test, a body behind a `{` where it reads one, with no end of
  /// input behind that body as evidence, a union's members behind an `=` where it reads them, and
  /// `repeatable` or `on` on a directive definition.
  fn next_ahead<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, found: &mut bool) {
    let head = peek_kind::<Src, Ctx>(inp)?;
    let word = head == Some(Kind::Identifier)
      && ends_at_word(owner, peek_as::<Src, Ctx, ContextualKeyword>(inp)?);
    match head {
      Some(Kind::LBrace) if asks(owner, Kind::LBrace) => {
        body_ahead::<Src, Ctx>(inp, open_at, owner, false, found)
      }
      Some(Kind::Equal) if asks(owner, Kind::Equal) => {
        members_ahead::<Src, Ctx>(inp, open_at, owner, found)
      }
      Some(Kind::Identifier) if word => {
        inp.try_expect_or_stop(|_| true)?;
        word_ahead::<Src, Ctx>(inp, found)
      }
      _ => behind_ahead::<Src, Ctx>(inp, open_at, true, found),
    }
  }

  /// A union's members ahead, at its `=`: a `|` or none, then names with a `|` between each two,
  /// each crossed by [`cross`]; and behind them what [`behind_ahead`] finds. Where a name is
  /// missing or refused, nothing is found.
  fn members_ahead<'inp, Src, Ctx>(inp, open_at: usize, owner: impl Header, found: &mut bool) {
    inp.try_expect_or_stop(|_| true)?;
    if peek_kind::<Src, Ctx>(inp)? == Some(Kind::Pipe) {
      inp.try_expect_or_stop(|_| true)?;
    }
    loop {
      let mut written = false;
      name_crossed::<Src, Ctx>(inp, owner, &mut written)?;
      if !written {
        return Ok(());
      }
      if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Pipe) {
        break;
      }
      inp.try_expect_or_stop(|_| true)?;
    }
    behind_ahead::<Src, Ctx>(inp, open_at, true, found)
  }

  /// The word its owner reads next, ahead, the word taken: [`word_goes_on`]'s test of the token
  /// behind it, read across any trivia. The end of input behind the word is no evidence.
  fn word_ahead<'inp, Src, Ctx>(inp, found: &mut bool) {
    if let Some(kind) = peek_kind::<Src, Ctx>(inp)? {
      *found = word_goes_on(kind, peek_as::<Src, Ctx, ContextualKeyword>(inp)?);
    }
    Ok(())
  }

  /// A name ahead, as a directive's name and a union's member are written, crossed by [`cross`]:
  /// `written` says whether one stands there and is not refused.
  fn name_crossed<'inp, Src, Ctx>(inp, owner: impl Header, written: &mut bool) {
    if peek_kind::<Src, Ctx>(inp)? != Some(Kind::Identifier) {
      return Ok(());
    }
    let mut crossing = Crossing::new();
    let mut refused = false;
    cross::<Src, Ctx>(inp, owner, &mut crossing, false, &mut refused)?;
    *written = !refused;
    Ok(())
  }
}
