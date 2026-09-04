//! The lossless lexer both dialects ship, declared once.

use tokora::{
  Lexer, ReadFrontier, Source, Token,
  lexer::{FromLogos, LogosLexer},
};

/// The lexer behind both dialects' `LosslessLexer` — a newtype over
/// [`LogosLexer`] that publishes [`Lexer`] and nothing else.
///
/// Each dialect aliases this at its own vocabulary, which is the name to reach it by:
/// [`graphql::lossless::LosslessLexer`](crate::graphql::lossless::LosslessLexer) and
/// [`graphqlx::lossless::LosslessLexer`](crate::graphqlx::lossless::LosslessLexer). Drive it
/// through [`Lexer`]: `new` / `with_state`, then `lex` until it returns `None`.
///
/// # Why this type exists at all
///
/// Both aliases used to name [`LogosLexer`] directly:
///
/// ```text
/// pub type LosslessLexer<'a, S = &'a str> = tokora::lexer::LogosLexer<'a, LosslessToken<S>>;
/// ```
///
/// A type alias is transparent, so that spelling published **every** inherent method
/// [`LogosLexer`] has — three of which hand back the raw `logos::Lexer` underneath it:
/// `into_inner`, `inner` and `inner_mut`.
///
/// The token ceiling is enforced in [`LogosLexer`]'s [`lex`](Lexer::lex): one post-scan
/// [`check`](Lexer::check) whose failure both reports the refusal and **latches**, so every later
/// call answers `None`. **Driving the raw `logos::Lexer` runs neither.** `logos::Lexer` is an
/// `Iterator`, so a caller who took `into_inner()` and iterated scanned to end of input no matter
/// what ceiling was configured — and
/// [`LosslessLimits::max_tokens`](crate::limits::LosslessLimits::max_tokens) says the lex stops one
/// lexeme after the ceiling.
///
/// Measured on 4 000 `-` at `with_max_tokens(0)`, in both dialects and at both source types: the
/// checked door produced **1** item and latched, the raw door produced **4 000**. The hooks in
/// `crate::handlers` are what make the counts differ — the first lexeme charges and the refusal
/// rides out on it, and every lexeme after that fails the hooks' pre-check *without* charging,
/// which returns `Err` to `logos`, and `logos` advances and yields it.
///
/// # Why a newtype, and not a latch inside `LosslessLimits`
///
/// The other repair on offer was to make an exceeded
/// [`LosslessLimits`](crate::limits::LosslessLimits) terminal, so that a raw lexer driven past the
/// ceiling stopped on its own. It cannot be done, for two independent reasons:
///
/// - **`logos::Lexer::extras` is a public field.** The state such a latch would live in *is* the
///   `Extras`, and a caller holding the raw lexer assigns over it: `raw.extras =
///   LosslessLimits::default()`. The latch's own home is caller-writable through the very handle
///   that escaped, so it could never be terminal.
/// - **A `logos` callback cannot end the stream.** Its outcomes are exactly emit, skip and error
///   (`logos::Filter`, `logos::FilterResult`, `logos::Skip`); there is no halt. Returning `Err`
///   yields `Some(Err(_))` and the lexer advances regardless — which is the *mechanism* of the
///   defect rather than a repair for it. The only approximation available, bumping to end of input
///   from inside the callback, would rewrite the refusal's span on the checked path too, and that
///   span is what smear issue #192 has just finished pinning.
///
/// Removing the door is what closes it. This is a **breaking** change to both dialects' public
/// API: the three raw accessors are gone. They had no call site anywhere in this workspace — its
/// tests, benches and examples reach the lossless lexers entirely through [`Lexer`], which is why
/// the trait impl below is the whole of what got re-exposed. (`crate::simd` does call
/// `LogosLexer::inner`, on the syntactic number sub-lexer; that names tokora's type directly and
/// is untouched.)
///
/// A caller determined to lex a `LosslessToken` outside smear's lexer can still name `logos` and
/// build their own. That is no longer smear's API making a promise it does not keep.
///
/// # One type, two dialects
///
/// The delegation is identical for GraphQL and GraphQLx, and a guarantee that lands in one dialect
/// and dies in the other is this workspace's most-repeated defect. Declaring it once means the two
/// cannot drift.
///
/// The parameter is the **token**, mirroring [`LogosLexer`]'s own, and deliberately not the slice
/// type each dialect's alias takes. A struct carries its bounds to every site that names it, while
/// a `pub type` does not check them at all — so parameterising by the token keeps
/// `LosslessToken<S>: FromLogos<'inp>` exactly where it already was, on the alias, and the ~200
/// generic signatures in `smear-parser` that name these lexers are unchanged.
#[cfg_attr(
  feature = "graphql",
  doc = r#"
# The three doors are gone, one fence each

Coded, and with a control beneath them, for the reason `smear-parser`'s crate root gives: a bare
`compile_fail` is satisfied by *any* failure, so a renamed `with_state` or a moved `LosslessLimits`
would keep one green while never reaching the accessor it claims to pin.

**One fence per accessor, deliberately.** Three calls in one block would be satisfied by whichever
failed first, so restoring exactly one of the three would leave the block red and say nothing. Split
this way, restoring any single accessor turns exactly one fence green — which is the failure the
fence is for.

```compile_fail,E0599
# use smear_lexer::{graphql::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let lexer = LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `into_inner` found
let _raw = lexer.into_inner();
```

```compile_fail,E0599
# use smear_lexer::{graphql::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let lexer = LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `inner` found
let _raw = lexer.inner();
```

```compile_fail,E0599
# use smear_lexer::{graphql::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let mut lexer =
  LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `inner_mut` found
let _raw = lexer.inner_mut();
```

The control, over the same paths, which must compile — and which is also the guarantee itself:
three `-` under a ceiling of zero stop after the one lexeme that carries the refusal.

```
# use smear_lexer::{graphql::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let mut lexer =
  LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
let mut items = 0usize;
while lexer.lex().is_some() {
  items += 1;
}
assert_eq!(items, 1);
```
"#
)]
#[cfg_attr(
  feature = "graphqlx",
  doc = r#"
# The same four, at GraphQLx

Not a copy for symmetry's sake. Both dialects aliased `LogosLexer`, so both published all three
accessors, and a repair proven in one dialect and assumed in the other is the defect this workspace
re-finds most often. These are what make the GraphQLx half a checked claim rather than an assumed
one.

```compile_fail,E0599
# use smear_lexer::{graphqlx::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let lexer = LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `into_inner` found
let _raw = lexer.into_inner();
```

```compile_fail,E0599
# use smear_lexer::{graphqlx::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let lexer = LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `inner` found
let _raw = lexer.inner();
```

```compile_fail,E0599
# use smear_lexer::{graphqlx::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let mut lexer =
  LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
// error[E0599]: no method named `inner_mut` found
let _raw = lexer.inner_mut();
```

```
# use smear_lexer::{graphqlx::lossless::LosslessLexer, limits::LosslessLimits, tokora::Lexer as _};
let mut lexer =
  LosslessLexer::<&str>::with_state("---", LosslessLimits::default().with_max_tokens(0));
let mut items = 0usize;
while lexer.lex().is_some() {
  items += 1;
}
assert_eq!(items, 1);
```
"#
)]
#[repr(transparent)]
pub struct LosslessLexer<'inp, T: FromLogos<'inp>>(LogosLexer<'inp, T>);

impl<'inp, T> Lexer<'inp> for LosslessLexer<'inp, T>
where
  T: FromLogos<'inp>,
  LogosLexer<'inp, T>: Lexer<'inp>,
{
  type State = <LogosLexer<'inp, T> as Lexer<'inp>>::State;
  type Source = <LogosLexer<'inp, T> as Lexer<'inp>>::Source;
  type Token = <LogosLexer<'inp, T> as Lexer<'inp>>::Token;
  type Span = <LogosLexer<'inp, T> as Lexer<'inp>>::Span;
  type Offset = <LogosLexer<'inp, T> as Lexer<'inp>>::Offset;

  /// Forwarded, not restated. `Sink::new`'s compile-time wall reads this, and a wrapper that
  /// answered for itself could disagree with the vocabulary it wraps.
  const SURFACES_TRIVIA: bool = <LogosLexer<'inp, T> as Lexer<'inp>>::SURFACES_TRIVIA;

  #[inline(always)]
  fn new(src: &'inp Self::Source) -> Self {
    Self(LogosLexer::new(src))
  }

  #[inline(always)]
  fn with_state(src: &'inp Self::Source, state: Self::State) -> Self {
    Self(LogosLexer::with_state(src, state))
  }

  #[inline(always)]
  fn check(&self) -> Result<(), <Self::Token as Token<'inp>>::Error> {
    self.0.check()
  }

  #[inline(always)]
  fn state(&self) -> &Self::State {
    self.0.state()
  }

  #[inline(always)]
  fn state_mut(&mut self) -> &mut Self::State {
    self.0.state_mut()
  }

  #[inline(always)]
  fn into_state(self) -> Self::State {
    self.0.into_state()
  }

  #[inline(always)]
  fn source(&self) -> &'inp Self::Source {
    self.0.source()
  }

  #[inline(always)]
  fn span(&self) -> Self::Span {
    self.0.span()
  }

  #[inline(always)]
  fn slice(&self) -> <Self::Source as Source<Self::Offset>>::Slice<'inp> {
    self.0.slice()
  }

  /// The checked door, and now the **only** door: one post-scan [`check`](Lexer::check) whose
  /// failure latches the adapter. What this newtype buys is that a caller cannot step around it.
  #[inline(always)]
  fn lex(&mut self) -> Option<Result<Self::Token, <Self::Token as Token<'inp>>::Error>> {
    self.0.lex()
  }

  #[inline(always)]
  fn read_frontier(&self) -> ReadFrontier<Self::Offset> {
    self.0.read_frontier()
  }

  #[inline(always)]
  fn bump(&mut self, n: &Self::Offset) {
    self.0.bump(n);
  }
}
