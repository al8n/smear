//! The trivia-aware atom set — the **only** peek/expect door the lossless productions use.
//!
//! Over a trivia-surfacing stream every decision point must commit leading trivia before it
//! looks at the next token. Committing trivia during a peek is safe: trivia belongs to the
//! parse, and to the tree, no matter which branch wins. Every atom here opens with that skip
//! so a production cannot forget it without bypassing this module.
//!
//! # What counts as trivia
//!
//! The atoms cross whatever [`Token::is_trivia`] admits, which for the lossless GraphQL token
//! is `smear-lexer`'s own `LosslessToken::is_trivia` — eight token forms:
//!
//! | Form | Bytes |
//! |---|---|
//! | `Bom` | `\u{FEFF}` |
//! | `Space` | `` ` ` `` |
//! | `Tab` | `\t` |
//! | `Newline` | `\n` |
//! | `CarriageReturn` | `\r` |
//! | `CarriageReturnAndNewline` | `\r\n` |
//! | `Comma` | `,` |
//! | `Comment` | `#` to the line terminator, terminator excluded |
//!
//! That is exactly GraphQL's `Ignored` production, and it is the same set `apollo-parser`
//! ignores — `is_whitespace_assimilated` (`lexer/mod.rs:602`) folds the BOM, the two space
//! forms and the two line terminators into one `Whitespace`, and `skip_ignored`
//! (`parser/mod.rs:236`) adds `Comment` and `Comma`. The sets agree; only the granularity
//! differs, and the coarser one is apollo's. `tests/lossless_trivia_atoms.rs` pins the set by
//! behaviour, form by form, so a lexer change that silently reclassifies one is a test failure
//! rather than a formatter bug found months later.
//!
//! **The comma is trivia, not punctuation.** GraphQL says so, and a reader coming from a
//! comma-separated language is the one most likely to misread it. It is not a separator any
//! production may require or count.
//!
//! # The kind vocabulary here is the lexer's, not the tree's
//!
//! `peek_kind` answers in `LosslessTokenKind` — `smear-lexer`'s vocabulary — and **not** in
//! [`SyntaxKind`](crate::graphql::kinds::SyntaxKind), the tree's. The two are different
//! spaces with overlapping variant names; a production compares an atom's answer against
//! `LosslessTokenKind`, and only [`super::kind_map`] ever speaks both.
//!
//! One consequence is worth stating because the obvious reading is wrong. The lexer's space
//! keeps `\r`, `\n` and `\r\n` apart where the tree's folds all three onto `SyntaxKind::Newline`
//! — but **no atom here can expose that difference**, because every atom skips trivia before
//! it answers, so a trivia kind is never a possible answer (pinned by
//! `peek_kind_never_answers_with_a_trivia_kind`). A consumer that needs to tell CRLF from LF
//! reads the token's text off the tree, which keeps it verbatim. The finer lexer kinds buy this
//! module nothing; they are simply what the projection happens to be.
//!
//! # Trivia commits eagerly, and that is a deliberate divergence
//!
//! `apollo-parser` queues ignored tokens in a `pending` buffer and flushes them into the tree
//! at the *next real token's* `eat` (`push_ignored`, `parser/mod.rs:243`), so trivia can land
//! inside a node that was opened after the trivia was read. These atoms commit trivia the
//! moment they cross it, which attaches it to whatever node is open **at the decision point** —
//! the outer one. That is the placement a formatter wants (the blank line before a field
//! belongs to the selection set, not to the field), and it is the placement tokora's sink gives
//! for free: a committed token lands in the innermost node open at its commit. Reproducing
//! apollo's deferral would mean a second buffering layer beside the sink's own mark/rollback
//! discipline, for a placement this suite does not want.

use tokora::{
  Lexer, SimpleSpan, Source, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  lexer::FromLogos,
  span::Spanned,
  try_parse_input::ParseAttempt,
};

use crate::graphql::GraphQL;

use super::{
  GraphqlLosslessError, GraphqlLosslessInput, GraphqlLosslessLexer, GraphqlLosslessToken,
};

/// `Token::kind`, reached without letting method resolution pick the wrong `Self`.
///
/// `skip_while` and `try_expect` hand their predicate a `Spanned<&Token, &Span>`, so `t.data()`
/// is a `&&Token`. At that receiver the inherent `LosslessToken::kind` (which wants `&Token`)
/// does not apply and the blanket `impl<'a, T: Token<'a>> Token<'a> for &'a T` does — which
/// ties the borrow's lifetime to `'inp` and makes the predicate's argument escape the closure.
/// Going through `t.data` (a `&Token`) instead reaches the *inherent* `kind`, whose return type
/// is the concrete `LosslessTokenKind` rather than the projection the atoms are generic over.
/// This helper is the one spelling that is both: the trait method, on the token itself.
///
/// `pub(crate)` because every predicate handed to `skip_while`, `try_expect` or
/// `sync_balanced` meets the same `&&Token` receiver — `recover.rs`'s sync predicate is the
/// second caller. One spelling, so the E0521 cannot be rediscovered per module.
#[inline]
pub(crate) fn kind_of<'a, T: Token<'a>>(token: &T) -> T::Kind {
  token.kind()
}

/// Commit any leading trivia, then report the next token's kind without consuming it.
/// `None` at end of input.
pub(crate) fn peek_kind<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
) -> Result<
  Option<<GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind>,
  GraphqlLosslessError<'inp, Src, Ctx>,
>
where
  Src: Source<usize> + ?Sized,
  // Both halves are load-bearing and neither is implied by `Lexer<'inp>`. `LosslessToken<S>`
  // gets its `Token` impl from `smear-lexer`'s `token!` macro, **once per concrete slice
  // type** — unlike `SyntacticToken<S>`, whose impl is generic (`syntactic/mod.rs:201`) — so
  // over a generic `Src` the projection `<Token>::Kind` has nothing to normalize against
  // without this bound. `FromLogos` sits on `LogosLexer`'s struct definition, which is what
  // makes the lexer alias nameable at all (see `document.rs`).
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  // `InputRef::peek_kind` needs this; the free `parser::peek_kind` needs
  // `Ctx: ComposableParseContext` instead, which is strictly stronger.
  GraphqlLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>,
{
  inp.skip_while(|t| t.is_trivia())?;
  inp.peek_kind()
}

/// Commit any leading trivia, then require `kind`.
///
/// On a mismatch the offending token is **left unconsumed**, at the cache front where
/// `try_expect` put it. That is the contract a lossless recovery needs: the caller's
/// `sync_to`/`sync_balanced` still gets to commit that token inside an `Error` node, so it
/// reaches the tree. Consuming it here — which is what `tokora::parser::expect` does, since it
/// reads through `next_or_stop` — would commit it to whatever node happens to be open, and the
/// recovery could only wrap the tokens after it.
pub(crate) fn expect<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<(), GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  // `Clone` on top of `peek_kind`'s pair: `UnexpectedToken::with_found` takes the token by
  // value, and the declined token is only ever borrowed (that is the whole point — it stays
  // unconsumed), so the diagnostic gets a copy.
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp> + Clone,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
  GraphqlLosslessError<'inp, Src, Ctx>: From<UnexpectedEot<usize, GraphQL>>
    + From<
      UnexpectedToken<
        'inp,
        GraphqlLosslessToken<'inp, Src>,
        <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
        SimpleSpan,
        GraphQL,
      >,
    >,
{
  inp.skip_while(|t| t.is_trivia())?;
  // There is no `expect_kind`. `try_expect` consumes-and-returns on a match and declines with
  // `None` otherwise, so the "or error" half is this function's job.
  match inp.try_expect(|t| kind_of(t.data) == kind)? {
    Some(_) => Ok(()),
    // The declined token is still at the cache front, so peeking it costs no re-lex and — the
    // point — does not consume it. `try_expect` also declines at genuine end of input, where
    // the peek is `None` and the right diagnostic is the end-of-input one, not "unexpected
    // token: <nothing>".
    None => Err(
      // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field
      // is already a reference, so the method form resolves to `<&Token as Clone>::clone` and
      // hands back another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and
      // fails the `From` bound a long way from here.
      match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
        Some(found) => UnexpectedToken::<_, _, _, GraphQL>::expected_one(found.span, kind)
          .with_found(found.data)
          .into(),
        None => UnexpectedEot::eot_of(inp.span().end()).into(),
      },
    ),
  }
}

/// Commit any leading trivia, then consume the next token only if it is `kind`.
///
/// A decline still commits the trivia it crossed — once. That is not a leak: the trivia was
/// read, it belongs to the tree, and the branch that wins next will not re-read it.
pub(crate) fn eat_if<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<bool, GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
{
  inp.skip_while(|t| t.is_trivia())?;
  // One `try_expect`, not a peek then an expect: a declining `try_expect` consumes nothing, so
  // this is already the conditional consume. Peeking first would read the same token twice.
  Ok(inp.try_expect(|t| kind_of(t.data) == kind)?.is_some())
}

/// [`eat_if`]'s declining form: commit any leading trivia, then consume the next token only if
/// it is `kind`, answering in [`ParseAttempt`] rather than `bool`.
///
/// # Why a second spelling of the same conditional consume
///
/// A [`ParseAttempt`] is what [`node_at`](tokora::parser::node_at) requires of its inner
/// parser: `NodeAt` implements [`TryParseInput`](tokora::TryParseInput) over a declining
/// parser, and spends the caller's mark **only** on `Accept`. That is the whole retro-wrap
/// mechanism — the mark is spent by the same call that finds the token justifying it, so no
/// statement (and no `?`) can come between the two and strand a spent-or-unspent mark. An
/// `eat_if` + unconditional wrap cannot express that: the token would be committed *outside*
/// the wrap's parser.
///
/// # The kind is a parameter, not a token this module names
///
/// The retro-wrap shapes each want a different token — `!` for `NonNullType`, `:` for an
/// `Alias` — and this module may not name a concrete dialect kind (the Lego rule). One atom
/// over the projection serves both; the call site closes over the kind it wants.
///
/// A decline still commits the trivia it crossed — once, exactly as [`eat_if`]'s does. That is
/// not a leak: the trivia was read, it belongs to the tree, and the branch that wins next will
/// not re-read it. It lands in whichever node is open at the decision point, which for a
/// declined retro-wrap is the enclosing node rather than the one that was never opened.
pub(crate) fn try_eat<'inp, Src, Ctx>(
  inp: &mut GraphqlLosslessInput<'inp, '_, Src, Ctx>,
  kind: <GraphqlLosslessToken<'inp, Src> as Token<'inp>>::Kind,
) -> Result<ParseAttempt<()>, GraphqlLosslessError<'inp, Src, Ctx>>
where
  Src: Source<usize> + ?Sized,
  GraphqlLosslessToken<'inp, Src>: Token<'inp> + FromLogos<'inp>,
  GraphqlLosslessLexer<'inp, Src>:
    Lexer<'inp, Token = GraphqlLosslessToken<'inp, Src>, Span = SimpleSpan, Offset = usize>,
  Ctx: tokora::ParseContext<'inp, GraphqlLosslessLexer<'inp, Src>, GraphQL>,
{
  inp.skip_while(|t| t.is_trivia())?;
  Ok(match inp.try_expect(|t| kind_of(t.data) == kind)? {
    Some(_) => ParseAttempt::Accept(()),
    None => ParseAttempt::Decline,
  })
}

/// Drivers that run one atom over a `&str` and report what it committed.
///
/// Test-only scaffolding, exported so the integration test at
/// `tests/lossless_trivia_atoms.rs` can reach it; nothing in the crate calls it.
///
/// **This module is where the atoms stop being generic.** The atoms above name only the
/// projections; these drivers must choose a concrete source, emitter and context to build a
/// `Sink` at all, exactly as [`super::runner::parse_str`] does.
#[doc(hidden)]
pub mod test_support {
  use std::{
    cell::Cell,
    string::{String, ToString},
  };

  use tokora::{
    InputRef, Parse as _, SimpleSpan,
    cache::DefaultCache,
    cst::Sink,
    emitter::{CstEmitter as _, Verbose},
  };

  use crate::graphql::{GraphQL, kinds::SyntaxKind as K};

  use super::super::{GraphqlLosslessErrors, GraphqlLosslessLexer, SyntaxNode, runner::profile};

  /// The kind vocabulary the atoms answer in — the lexer's, not the tree's.
  pub type Kind = smear_lexer::graphql::lossless::LosslessTokenKind;

  /// The recording emitter, the sink over it, the context pair, and the input the driver's
  /// closure receives.
  ///
  /// Spelled out because a closure's parameter type is **not** inferred through a
  /// `ParseInput` bound — only through an `Fn` bound — so `|inp: &mut _|` leaves `L` and `Ctx`
  /// unresolved and the body's first method call is the error site. `runner::parse_str` never
  /// hits this: it applies a named function whose own signature pins both.
  type Emitter<'inp> = Verbose<GraphqlLosslessErrors<&'inp str>, SimpleSpan, GraphQL>;
  type TestSink<'inp> = Sink<'inp, GraphqlLosslessLexer<'inp, str>, Emitter<'inp>>;
  type TestCtx<'inp, 'sink> = (
    &'sink mut TestSink<'inp>,
    DefaultCache<'inp, GraphqlLosslessLexer<'inp, str>>,
  );
  type TestInput<'inp, 'input, 'sink> =
    InputRef<'inp, 'input, GraphqlLosslessLexer<'inp, str>, TestCtx<'inp, 'sink>, GraphQL>;

  /// Runs one atom over `src` inside a `Document` node, drains whatever the atom left, and
  /// materializes.
  ///
  /// Two details carry the whole file's evidence:
  ///
  /// - **The atom runs inside a node**, so "what the atom committed" is a thing the *tree*
  ///   can be asked — it is that node's text — rather than a cursor offset the sink never saw.
  ///   A skip that fails to commit produces a shorter node, which is how the mutation proof in
  ///   Task 4 Step 5 goes red.
  /// - **The remainder is drained afterwards.** `Sink::finish` refuses any source byte that no
  ///   committed token covers and no lexer-error diagnostic explains
  ///   (`FinishError::UncoveredGap`), and an atom deliberately stops mid-source. Without the
  ///   drain every helper here would fail materialization instead of reporting its atom.
  macro_rules! drive {
    ($lt:lifetime, $src:expr, $init:expr, |$inp:ident| $atom:expr) => {{
      let src: &$lt str = $src;
      let out = Cell::new($init);

      let mut sink: TestSink<$lt> = Sink::new(src, Emitter::default(), profile::<str>());

      let _ = tokora::Parser::with_context::<GraphqlLosslessLexer<'_, str>, (), _>((
        &mut sink,
        DefaultCache::<GraphqlLosslessLexer<'_, str>>::default(),
      ))
      .apply::<_, GraphQL>(|$inp: &mut TestInput<$lt, '_, '_>| {
        // The `node` combinator's own `mark` / `start_at` / `finish`, spelled out. The
        // combinator wants an inner parser whose error type is pinned, and a closure whose
        // body is `Ok(())` pins nothing — so the wrap is driven directly instead of teaching
        // the test scaffold's inference about a type it never names.
        let mark = $inp.emitter().cst_mark();
        out.set($atom);
        $inp.emitter().cst_start_at(mark, K::Document.raw());
        $inp.emitter().cst_finish(K::Document.raw());
        // Whatever the atom left behind, so `finish` has full coverage.
        $inp.skip_while(|_| true)
      })
      .parse_str(src);

      let (green, _emitter) = sink.finish(K::Root.raw());
      let root = SyntaxNode::new_root(
        green.expect("the trivia-atom driver emitted a malformed event stream"),
      );
      // The `Document` node is the only node child; its text is what the atom committed.
      let text: String = root
        .first_child()
        .map(|n| n.text().to_string())
        .unwrap_or_default();

      (out.into_inner(), text)
    }};
  }

  /// `super::peek_kind` over `src`.
  ///
  /// `::<str, _>` for the reason `runner::parse_str` spells out: `str` and `&str` both project
  /// `Slice<'inp> = &'inp str`, so the lexer type alone leaves an atom's `Src` genuinely
  /// ambiguous, and `str` is the one that matches `L::Source`.
  ///
  /// The error is a **panic, not a `None`**. `peek_kind`'s `None` means end of input and a test
  /// asserts on it; folding an error into the same value would let a broken atom pass the
  /// end-of-input test for the wrong reason.
  pub fn peek_kind_of<'inp>(src: &'inp str) -> Option<Kind> {
    let (kind, _text) = drive!('inp, src, None, |inp| super::peek_kind::<str, _>(inp)
      .expect("peek_kind must not error over these fixtures"));
    kind
  }

  /// `super::expect` for `{` over `src`: did it match, and what did it commit?
  pub fn expect_brace_of<'inp>(src: &'inp str) -> (bool, String) {
    drive!('inp, src, false, |inp| super::expect::<str, _>(inp, Kind::LBrace).is_ok())
  }

  /// `super::eat_if` for `{` over `src`: did it eat, and what did it commit?
  ///
  /// Panics on an error for the same reason `peek_kind_of` does: `eat_if`'s `false` means
  /// "declined", which a test asserts on, so an error must not be able to spell it.
  /// `expect_brace_of` is the exception — its `bool` **is** `is_ok()`, since a failed `expect`
  /// erroring is the behaviour under test.
  pub fn eat_if_brace_of<'inp>(src: &'inp str) -> (bool, String) {
    drive!('inp, src, false, |inp| super::eat_if::<str, _>(inp, Kind::LBrace)
      .expect("eat_if must not error over these fixtures"))
  }
}
