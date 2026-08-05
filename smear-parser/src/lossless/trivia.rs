//! The trivia-aware atom set — the **only** peek/expect door a lossless production uses.
//!
//! Over a trivia-surfacing stream every decision point must commit leading trivia before it
//! looks at the next token. Committing trivia during a peek is safe: trivia belongs to the
//! parse, and to the tree, no matter which branch wins. Every atom here opens with that skip
//! so a production cannot forget it without bypassing this module.
//!
//! # What counts as trivia is the lexer's answer, not this module's
//!
//! The atoms cross whatever [`Token::is_trivia`](tokora::Token::is_trivia) admits. Which token
//! forms that is, is a **dialect** fact and is recorded in each dialect's own `trivia` module,
//! beside the wrappers that pin the parameters; this module cannot name a token form and must
//! not try.
//!
//! # The kind vocabulary is the lexer's, not the tree's
//!
//! [`peek_kind`] answers in `<L::Token as Token>::Kind` — the lexer's vocabulary — and **not** in
//! the dialect's `SyntaxKind`, the tree's. The two are different spaces with overlapping variant
//! names; a production compares an atom's answer against the lexer kind, and only a dialect's
//! `kind_map` ever speaks both.
//!
//! One consequence is worth stating because the obvious reading is wrong. A lexer space that
//! keeps `\r`, `\n` and `\r\n` apart where the tree's folds all three onto one `Newline` cannot
//! expose that difference **through any atom here**, because every atom skips trivia before it
//! answers, so a trivia kind is never a possible answer. A consumer that needs to tell CRLF from
//! LF reads the token's text off the tree, which keeps it verbatim.
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
//!
//! # Why the atoms are `pub`
//!
//! They are internal-facing — a consumer of the CST never calls one — but the cross-dialect
//! isolation gate needs a public surface to assert against, and a `pub(crate)` substrate cannot
//! be shown to be dialect-free from outside the crate. `#![deny(missing_docs)]` then makes that
//! a real doc-writing obligation rather than a note.

use tokora::{
  ErrorOf, InputRef, Lexer, SimpleSpan, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  lexer::FromLogos,
  span::Spanned,
  try_parse_input::ParseAttempt,
  utils::DowncastRef,
};

/// `Token::kind`, reached without letting method resolution pick the wrong `Self`.
///
/// `skip_while` and `try_expect` hand their predicate a `Spanned<&Token, &Span>`, so `t.data()`
/// is a `&&Token`. At that receiver a concrete token's *inherent* `kind` (which wants `&Token`)
/// does not apply and the blanket `impl<'a, T: Token<'a>> Token<'a> for &'a T` does — which
/// ties the borrow's lifetime to `'inp` and makes the predicate's argument escape the closure.
/// Going through `t.data` (a `&Token`) instead reaches the inherent `kind`, whose return type
/// is the concrete kind rather than the projection the atoms are generic over. This helper is
/// the one spelling that is both: the trait method, on the token itself.
///
/// Public because every predicate handed to `skip_while`, `try_expect` or `sync_balanced` meets
/// the same `&&Token` receiver — a dialect's recovery sync predicate is the second caller. One
/// spelling, so the `E0521` cannot be rediscovered per module.
#[inline]
pub fn kind_of<'a, T: Token<'a>>(token: &T) -> T::Kind {
  token.kind()
}

/// Commit any leading trivia, then report the next token's kind without consuming it.
/// `None` at end of input.
pub fn peek_kind<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<<L::Token as Token<'inp>>::Kind>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  // `FromLogos` sits on `LogosLexer`'s struct definition, which is what makes a logos-backed
  // lexer alias nameable at all. Generically, `L::Token` *is* the lexer's associated type, so
  // the `Kind = …` equality the dialect-bound signatures carried has nothing left to state.
  L::Token: FromLogos<'inp>,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
  // `InputRef::peek_kind` needs this; the free `parser::peek_kind` needs
  // `Ctx: ComposableParseContext` instead, which is strictly stronger.
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>,
{
  inp.skip_while(|t| t.is_trivia())?;
  inp.peek_kind()
}

/// Commit any leading trivia, then project the next token to `Projection` without consuming
/// it. `None` at end of input, and `None` for a token that carries no `Projection`.
///
/// # Why the two `None`s are not told apart
///
/// Every caller asks the same question — *is the head this particular keyword?* — and both a
/// head that spells something else and an absent head answer it the same way. Distinguishing
/// them would put a second `Option` at every call site to carry a difference none of them
/// branches on; the kind-level [`peek_kind`] is already the atom that reports end of input.
///
/// # The projection is a type parameter, and must be
///
/// A dialect's `true`, `false`, `null`, `on`, `query`, `fragment` and the rest are **contextual
/// keywords**: the lexer hands them back as ordinary identifier tokens, so a production that
/// needs one has to read the *spelling*, which [`peek_kind`] cannot see.
/// [`DowncastRef`](tokora::utils::DowncastRef) is that door — and naming its target here would
/// make this module name a concrete dialect type, which the Lego rule forbids. One atom over
/// the type parameter serves every projection a dialect defines; the call site names the one
/// it wants.
pub fn peek_as<'inp, L, Ctx, Lang, Projection>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<Option<Projection>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + DowncastRef<Projection>,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>,
{
  inp.skip_while(|t| t.is_trivia())?;
  // The projection is owned and `Copy`, so nothing borrowed escapes the closure — the `&&Token`
  // receiver that costs `kind_of` its own helper is harmless here. The outer `Option` is the
  // peek and the inner one is the downcast; see above for why they are flattened into one.
  Ok(inp.peek_head_map(|t| t.data.downcast_ref())?.flatten())
}

/// The diagnostic a failed [`expect`] carries: an unexpected-token error naming `kind`, or the
/// end-of-input error when there is no token to name.
///
/// Split out because `expect` needs the *same* value twice — once to emit and once to return —
/// and the parse context's error type is not `Clone` without a tenth clause in every
/// production's where-bundle. Building it twice costs a second peek, which is served straight
/// out of the cache and consumes nothing.
fn expectation_failure<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kind: <L::Token as Token<'inp>>::Kind,
) -> Result<Spanned<ErrorOf<'inp, L, Ctx, Lang>, SimpleSpan>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp> + Clone,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  // The declined token is still at the cache front, so peeking it costs no re-lex and — the
  // point — does not consume it. `try_expect` also declines at genuine end of input, where the
  // peek is `None` and the right diagnostic is the end-of-input one, not "unexpected token:
  // <nothing>".
  //
  // `Clone::clone(t.data)`, not `t.data.clone()`: `Spanned<&Token, &Span>`'s `data` field is
  // already a reference, so the method form resolves to `<&Token as Clone>::clone` and hands
  // back another borrow — which then infers `UnexpectedToken`'s `T` as `&Token` and fails the
  // `From` bound a long way from here.
  Ok(
    match inp.peek_head_map(|t| Spanned::new(*t.span, Clone::clone(t.data)))? {
      Some(found) => Spanned::new(
        found.span,
        UnexpectedToken::<_, _, _, Lang>::expected_one(found.span, kind)
          .with_found(found.data)
          .into(),
      ),
      None => {
        let end = inp.span().end();
        Spanned::new(SimpleSpan::new(end, end), UnexpectedEot::eot_of(end).into())
      }
    },
  )
}

/// Commit any leading trivia, then require `kind`.
///
/// On a mismatch the offending token is **left unconsumed**, at the cache front where
/// `try_expect` put it. That is the contract a lossless recovery needs: the caller's
/// `sync_to`/`sync_balanced` still gets to commit that token inside an `Error` node, so it
/// reaches the tree. Consuming it here — which is what `tokora::parser::expect` does, since it
/// reads through `next_or_stop` — would commit it to whatever node happens to be open, and the
/// recovery could only wrap the tokens after it.
///
/// # A mismatch is **emitted** as well as returned, and that is load-bearing
///
/// The `Err` this returns is not a diagnostic: it unwinds to the dialect's document production,
/// which catches it, resynchronises and continues — deliberately *without* reporting, since the
/// failure's own position is here and not there. Emitting at the point of failure is therefore
/// the only report the failure ever gets, and `Parse::has_errors` is the verdict the
/// acceptance-parity gate compares against `syntactic/`. Returning `Err` alone left a failed
/// parse reading as a clean one — a defect a Phase A task found and had to own.
pub fn expect<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kind: <L::Token as Token<'inp>>::Kind,
) -> Result<(), ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  // `Clone` on top of `peek_kind`'s: `UnexpectedToken::with_found` takes the token by value, and
  // the declined token is only ever borrowed (that is the whole point — it stays unconsumed), so
  // the diagnostic gets a copy.
  L::Token: FromLogos<'inp> + Clone,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<usize, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, SimpleSpan, Lang>>,
{
  inp.skip_while(|t| t.is_trivia())?;
  // There is no `expect_kind`. `try_expect` consumes-and-returns on a match and declines with
  // `None` otherwise, so the "or error" half is this function's job.
  match inp.try_expect(|t| kind_of(t.data) == kind)? {
    Some(_) => Ok(()),
    None => {
      // Emit first, then return the same diagnostic as the `Err`. Built twice rather than
      // cloned: see [`expectation_failure`].
      let reported = expectation_failure::<L, Ctx, Lang>(inp, kind)?;
      inp.emit_error(reported)?;
      Err(expectation_failure::<L, Ctx, Lang>(inp, kind)?.data)
    }
  }
}

/// Commit any leading trivia, then consume the next token only if it is `kind`.
///
/// A decline still commits the trivia it crossed — once. That is not a leak: the trivia was
/// read, it belongs to the tree, and the branch that wins next will not re-read it.
pub fn eat_if<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kind: <L::Token as Token<'inp>>::Kind,
) -> Result<bool, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
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
/// The retro-wrap shapes each want a different token — `!` for a non-null type, `:` for a field
/// alias — and this module may not name a concrete dialect kind (the Lego rule). One atom over
/// the projection serves both; the call site closes over the kind it wants.
///
/// A decline still commits the trivia it crossed — once, exactly as [`eat_if`]'s does. That is
/// not a leak: the trivia was read, it belongs to the tree, and the branch that wins next will
/// not re-read it. It lands in whichever node is open at the decision point, which for a
/// declined retro-wrap is the enclosing node rather than the one that was never opened.
pub fn try_eat<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kind: <L::Token as Token<'inp>>::Kind,
) -> Result<ParseAttempt<()>, ErrorOf<'inp, L, Ctx, Lang>>
where
  Lang: ?Sized,
  L: Lexer<'inp, Span = SimpleSpan, Offset = usize>,
  L::Token: FromLogos<'inp>,
  Ctx: tokora::ParseContext<'inp, L, Lang>,
{
  inp.skip_while(|t| t.is_trivia())?;
  Ok(match inp.try_expect(|t| kind_of(t.data) == kind)? {
    Some(_) => ParseAttempt::Accept(()),
    None => ParseAttempt::Decline,
  })
}
