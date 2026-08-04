//! Recovery: a grammar error becomes a diagnostic and, where there is anything to attribute,
//! an `Error` node; the parse then continues. `finish` gap-tiles anything left uncovered, so
//! text fidelity holds regardless.
//!
//! Built on tokora's sync family, not on hand-rolled skip loops. The members behave
//! differently and the differences decide which one each helper uses:
//!
//! - `sync_balanced(classifier, pred)` — **two** args: `classifier` names which kinds open and
//!   close pairs (`DelimClass`), `pred` is the depth-0 sync predicate. On a **successful** sync
//!   that skipped ≥1 token it reports the region once through `emit_skipped_region`, and the
//!   sink wraps those tokens in the profile's `error_kind` **automatically** — no explicit
//!   `node` call.
//! - `sync_balanced` **makes no progress in two cases**, and both are reachable here. A
//!   no-match run to end of input "commits nothing and returns `Ok(None)`, leaving no trace";
//!   and a *successful* sync whose predicate matches the very first token returns
//!   `Some(Hole { skipped: 0 })` — the sync point was already at hand — consuming nothing and
//!   emitting nothing. See `unexpected` for why that pair is a termination hazard rather than
//!   a curiosity.
//! - `sync_to` / `sync_through` — also two args, and they do **not** auto-wrap, so a caller
//!   that wants the skipped tokens inside a node must open one itself. Task 8's top-level
//!   resync is their caller; nothing in Task 5 reaches them, and the plan's
//!   `resync_to_definition` is therefore deferred to the task that can test it at its own call
//!   site.
//!
//! # Every helper called from inside a loop must consume at least one token
//!
//! A helper that returns `Ok` without consuming turns its caller's `while` into an infinite
//! loop. `unexpected` is in that class and guarantees progress explicitly; `unclosed_list` and
//! `unclosed_object` are not, because their call sites `return` out of the loop rather than
//! falling through it. The distinction is not "does it consume" but "does the call site
//! continue".
//!
//! **Do not write `node(K::Error.raw(), |_| Ok(()))`.** It is a no-op: an empty, zero-width
//! `Error` node that consumes nothing, which is the rule above violated in the one place it
//! looks like recovery.

use smear_lexer::graphql::{ContextualKeyword, lossless::LosslessTokenKind as Kind};
use tokora::{
  SimpleSpan,
  emitter::FromUnclosed,
  error::{UnclosedBrace, UnclosedBracket, UnclosedParen, UnexpectedEot, token::UnexpectedToken},
  input::Balance,
  span::Spanned,
  utils::DowncastRef,
};

use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

use super::{GraphqlLosslessError, GraphqlLosslessLexer, trivia::kind_of};

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

/// The span of the single-byte delimiter an [`expect`] has just
/// committed, given the input's committed extent.
///
/// `expect` reports only whether it matched, so the opener's own span has to be recovered from
/// the input: its end is the delimiter's end, and every delimiter this suite opens (`[`, `{`,
/// `(`) is exactly one byte. That span is what an unclosed-delimiter diagnostic points at — the
/// opener that was never closed, not the end of input where the absence was noticed.
#[inline]
pub(crate) fn opener_span(end: usize) -> SimpleSpan {
  SimpleSpan::new(end.saturating_sub(1), end)
}

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

/// [`kind_of`](super::trivia::kind_of)'s twin for the spelling: `DowncastRef`, reached without
/// letting method resolution pick the wrong `Self`.
///
/// `sync_balanced` hands its predicate a `Spanned<&Token, &Span>`, so the same `&&Token`
/// receiver that costs `kind_of` its own helper applies here. The projection is owned and
/// `Copy`, so nothing borrowed escapes.
#[inline]
fn keyword_of<T: DowncastRef<ContextualKeyword>>(token: &T) -> Option<ContextualKeyword> {
  token.downcast_ref()
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

lossless_production! {
  /// A list ran to end of input before its `]` arrived.
  ///
  /// `open` is the opening `[`'s span, which is where the diagnostic points — the closer that
  /// never came has no position of its own.
  ///
  /// **This helper opens no node and consumes nothing, and that is correct rather than a
  /// shortcoming.** Its only caller reaches it when the atom set reported end of input, so
  /// there is no token left to skip, nothing to attribute to an `Error` node, and nothing for
  /// `sync_balanced` to settle: at end of input it would commit nothing, wrap nothing and emit
  /// no hole diagnostic. Calling it here would be dead code that *reads* as the mechanism.
  ///
  /// **Loop safety does not depend on consuming here.** The caller's `while` is guarded by an
  /// end-of-input test and this helper's result is `return`ed out of that loop rather than
  /// continuing it, so there is no iteration to starve. The missing closer's absence is
  /// recorded in the diagnostic; the source bytes are already accounted for by the tokens
  /// committed before end of input.
  fn unclosed_list<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    let err = <GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<
      'inp,
      GraphqlLosslessLexer<'inp, Src>,
      GraphQL,
    >>::from_unclosed(UnclosedBracket::<SimpleSpan, GraphQL>::bracket_of(open));
    inp.emit_error(Spanned::new(open, err))?;
    Ok(())
  }

  /// An object ran to end of input before its `}` arrived — `unclosed_list`'s twin, and the
  /// same reasoning applies clause for clause.
  fn unclosed_object<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    let err = <GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<
      'inp,
      GraphqlLosslessLexer<'inp, Src>,
      GraphQL,
    >>::from_unclosed(UnclosedBrace::<SimpleSpan, GraphQL>::brace_of(open));
    inp.emit_error(Spanned::new(open, err))?;
    Ok(())
  }

  /// An argument list ran to end of input before its `)` arrived — the third of the same twin,
  /// and the reason [`FromUnclosed`] is generic over the delimiter marker: one impl in
  /// `lossless/mod.rs` covers `[]`, `{}` and `()`, so a new pair costs a constructor call and
  /// no new bound.
  fn unclosed_parens<'inp, Src, Ctx>(inp, open: SimpleSpan) {
    let err = <GraphqlLosslessError<'inp, Src, Ctx> as FromUnclosed<
      'inp,
      GraphqlLosslessLexer<'inp, Src>,
      GraphQL,
    >>::from_unclosed(UnclosedParen::<SimpleSpan, GraphQL>::paren_of(open));
    inp.emit_error(Spanned::new(open, err))?;
    Ok(())
  }

  /// Nothing that could start one of `expected` is here. **Report, and consume nothing.**
  ///
  /// # When to reach for this rather than [`unexpected`]
  ///
  /// Whenever the offending token is worth more to the *next* production than it would be
  /// inside an `Error` node. Three shapes in this suite are in that class, and each one is a
  /// bug if it consumes:
  ///
  /// - **A required keyword is absent**, as with a type condition's `on`. The name that *is*
  ///   there is still the condition's type, so eating it would trade one diagnostic for a lost
  ///   subtree.
  /// - **A delimited shape is empty** where the grammar says `+` — `{}`, `()`. The report
  ///   points at the closer, which the enclosing loop is about to eat as its own.
  /// - **A prefix has no tail**, as with a `...` at the end of a selection set. The `}` after
  ///   it belongs to the enclosing set; [`unexpected`] would swallow it (a closer *is* a sync
  ///   point) and the set would then run to end of input looking for a closer it had already
  ///   consumed.
  ///
  /// **Consuming nothing makes this unsafe inside a loop that continues.** Every caller here
  /// either `return`s afterwards or has already made progress on its own — the same
  /// distinction [`unclosed_list`] documents, and the reason [`unexpected`] exists at all.
  fn report_unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field is
    // already a reference, so the method form resolves to `<&Token as Clone>::clone` and hands
    // back another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and fails the
    // `From` bound a long way from here.
    match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
      Some(found) => {
        let span = found.span;
        let err = UnexpectedToken::<_, _, _, GraphQL>::expected_one_of(span, expected)
          .with_found(found.data);
        inp.emit_error(Spanned::new(span, err.into()))?;
      }
      None => {
        let end = inp.span().end();
        let span = SimpleSpan::new(end, end);
        let err = UnexpectedEot::<usize, GraphQL>::eot_of(end);
        inp.emit_error(Spanned::new(span, err.into()))?;
      }
    }
    Ok(())
  }

  /// Nothing that could start one of `expected` is here, and there is still input.
  ///
  /// Reports once through [`report_unexpected`], then makes progress — in that order, because
  /// the diagnostic names the token that is about to be skipped.
  ///
  /// # Why this cannot be `sync_balanced` alone
  ///
  /// The plan's draft was a bare `inp.sync_balanced(…)`, on the reasoning that a balanced skip
  /// both makes progress and reports itself. It does neither reliably, and the two gaps are
  /// exactly the inputs a recovery path meets:
  ///
  /// - **A stray closer is itself a sync point.** At depth zero `pred` is consulted *first*,
  ///   so over `[1 ) 2]` the scan matches the `)` the caller is standing on, returns
  ///   `Some(Hole { skipped: 0 })` and consumes nothing. The caller's `while` then re-reads
  ///   that same `)` — forever.
  /// - **Garbage running to end of input never matches.** Over `[1 ! ! !` there is no sync
  ///   point, so the scan rewinds wholesale and returns `Ok(None)`. Same spin.
  ///
  /// So the skip is *attempted first* — it is the good outcome: one hole diagnostic for a
  /// whole garbage run, nesting-aware, wrapped by the sink itself — and a fallback consumes
  /// exactly one token into an `Error` node when the skip made no progress. Progress is then
  /// unconditional whenever input remains, which is the enclosing loop's whole safety
  /// argument.
  ///
  /// The fallback node is opened **only once the token is in hand**, by hand rather than
  /// through the `node` combinator, so a caller that reaches this at genuine end of input gets
  /// no node at all rather than an empty zero-width `Error` one.
  fn unexpected<'inp, Src, Ctx>(inp, expected: &'static [Kind]) {
    report_unexpected::<Src, Ctx>(inp, expected)?;

    let hole = inp.sync_balanced(delimiters, |t| is_sync_point(kind_of(t.data)))?;
    if hole.is_some_and(|h| h.skipped() > 0) {
      // The sink wrapped the skipped region in the profile's `error_kind` on its own.
      return Ok(());
    }

    let mark = inp.cst_mark();
    if inp.try_expect(|_| true)?.is_some() {
      inp.cst_start_at(mark, K::Error.raw());
      inp.cst_finish(K::Error.raw());
    }
    Ok(())
  }

  /// A definition returned `Err`: skip its wreckage and stop before the next definition head.
  ///
  /// # This helper reports nothing, and that is the point
  ///
  /// Its only callers are the two document loops, and they reach it *because* a production
  /// failed — which means [`expect`] already emitted at the position the
  /// failure happened. A second diagnostic here would point at whatever the resync happens to
  /// start on, which is not where anything went wrong.
  ///
  /// # The fallback is **not** [`unexpected`]'s, and copying it there was a defect
  ///
  /// [`unexpected`] consumes one token whenever its balanced skip made no progress, because its
  /// caller has already ruled the token at hand junk. Here the opposite is true: a scan that
  /// stops having skipped **zero** tokens has stopped *on a definition head* — the exact token
  /// this helper went looking for — and eating it costs the whole definition. `type T { a scalar
  /// S }` loses its `ScalarTypeDefinition` that way, which is what
  /// `a_resync_that_lands_on_a_definition_head_does_not_eat_it` pins.
  ///
  /// So the outcomes are told apart rather than lumped together:
  ///
  /// - **`Some(hole)`** — a definition head is at hand, whether the scan crossed anything to
  ///   reach it or not. Whatever it crossed is already wrapped in `error_kind` by the sink;
  ///   return, and the caller's next turn parses the head.
  /// - **`None`** — the scan ran to end of input without finding one, rewound, and committed
  ///   nothing. Everything left is junk, so consuming one token into an `Error` node can eat no
  ///   definition, and it is the branch that keeps this helper self-sufficient.
  ///
  /// # Termination does not rest on that fallback
  ///
  /// The document loops' dispatchers consume at least one token before they can fail — a
  /// description commits its string, every keyword arm commits its keyword, and the fall-through
  /// is [`unexpected`], which guarantees progress on its own — so a resync that consumes nothing
  /// cannot starve the loop. Deleting the `None` branch is therefore **not** observable as a
  /// hang; it is kept because a helper whose safety depends on a caller's internals is one
  /// refactor away from being wrong, and because a token nobody attributes is a token that
  /// reaches the tree as a loose child of `Document`.
  ///
  /// The leading peek is not decoration either. `sync_balanced` counts *every* token into its
  /// hole, trivia included, so trivia the failed production left uncrossed would otherwise land
  /// inside the `Error` node instead of beside it.
  fn resync_to_definition<'inp, Src, Ctx>(inp) {
    if super::trivia::peek_kind::<Src, Ctx>(inp)?.is_none() {
      return Ok(());
    }

    if inp
      .sync_balanced(delimiters, |t| {
        is_definition_start(kind_of(t.data), keyword_of(t.data))
      })?
      .is_some()
    {
      return Ok(());
    }

    let mark = inp.cst_mark();
    if inp.try_expect(|_| true)?.is_some() {
      inp.cst_start_at(mark, K::Error.raw());
      inp.cst_finish(K::Error.raw());
    }
    Ok(())
  }
}
