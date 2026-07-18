//! The GraphQLx dialect's grammar productions.
//!
//! Each production is a free fn generic over `L: Lexer<'inp>` — bounded only by
//! tokora capability traits and the [`ParseCtx`](crate::combinator::ParseCtx)
//! bundle — with the `Lang` type parameter left generic and `Span = SimpleSpan`,
//! exactly as the GraphQL dialect (Amendment 1). The productions assemble the
//! [atom layer](crate::combinator) into the dialect's typed AST nodes and, at every
//! node boundary, record a `node(kind)` event that is a no-op on the syntactic
//! (`Fatal`/`Verbose`) emitters and materializes a lossless CST node on a recording
//! sink — the same production set, never a twin.
//!
//! The module name reflects the driving lexer the syntactic suite pairs these
//! productions with; the lossless suite (a later wave) drives the *same* fns over a
//! trivia-surfacing lexer and a `cst::Sink`, swapping `L` and the emitter alone.

pub mod value;

use tokora::{InputRef, Lexer};

/// Peeks the next token (without consuming it) and reports whether it satisfies
/// `pred`. `false` on end of input.
///
/// The one-token, non-consuming dispatch primitive the value and type productions
/// reach for when they must decide a fork on the shape of the next token — a `set`
/// followed by `{` vs a bare enum path, a path followed by `<` vs a plain path —
/// while leaving that token in place for the committed arm. Mirrors the closure
/// discipline of [`value`](value::value)'s `classify_value_head`.
// `ParseCtx`/`ErrorOf` are fully qualified rather than imported so the module doc's
// `[`ParseCtx`](crate::combinator::ParseCtx)` link stays a non-redundant explicit target.
fn peeks_where<'inp, L, Ctx, Lang, F>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  pred: F,
) -> Result<bool, crate::combinator::ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  Ctx: crate::combinator::ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  F: Fn(&L::Token) -> bool,
{
  let mut found = false;
  inp.try_expect(|spanned| {
    found = pred(spanned.data);
    false
  })?;
  Ok(found)
}
