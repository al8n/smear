//! Token atoms: committed and declining parsers for GraphQL punctuators, for
//! identifiers, and for keywords.
//!
//! Each atom is a thin wrapper over the matching tokora parser. The committed form
//! (e.g. [`at`], [`ident`], [`keyword_exact`]) consumes the token and errors on
//! anything else; the declining form (e.g. [`try_at`], [`try_ident`],
//! [`try_keyword_exact`]) consumes it only when it is next and otherwise declines
//! without touching the input.
//!
//! [`typed_keyword_atom`] turns a spelling into a committed/declining atom pair
//! that maps the matched keyword onto a dialect's typed keyword node; the macro
//! definition stays generic, so a dialect assembly supplies the concrete node
//! type at the invocation site.

use tokora::{
  InputRef, Lexer, ParseInput, Token, TryParseInput,
  error::{UnexpectedEot, token::UnexpectedToken},
  token::{IdentifierToken, KeywordToken, PunctuatorToken},
  try_parse_input::ParseAttempt,
};

use super::{Equivalent, ErrorOf, ParseCtx, SliceOf};

macro_rules! punct_atoms {
  ($($fn_name:ident / $try_fn_name:ident => $Ty:ident),+ $(,)?) => {
    $(
      #[doc = concat!("Commits to the `", stringify!($Ty), "` punctuator; errors on anything else.")]
      #[inline]
      pub fn $fn_name<'inp, L, Ctx, Lang>(
        inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
      ) -> Result<tokora::punct::$Ty<L::Span, (), Lang>, ErrorOf<'inp, L, Ctx, Lang>>
      where
        L: Lexer<'inp>,
        L::Token: PunctuatorToken<'inp>,
        Ctx: ParseCtx<'inp, L, Lang>,
        Lang: ?Sized,
        ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
          + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
      {
        tokora::punct::$Ty::parse_of(inp)
      }

      #[doc = concat!("Declines (no tokens consumed) unless the next token is `", stringify!($Ty), "`.")]
      #[inline]
      pub fn $try_fn_name<'inp, L, Ctx, Lang>(
        inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
      ) -> Result<ParseAttempt<tokora::punct::$Ty<L::Span, (), Lang>>, ErrorOf<'inp, L, Ctx, Lang>>
      where
        L: Lexer<'inp>,
        L::Token: PunctuatorToken<'inp>,
        Ctx: ParseCtx<'inp, L, Lang>,
        Lang: ?Sized,
        ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
      {
        tokora::punct::$Ty::try_parse_of(inp)
      }
    )+
  };
}

punct_atoms!(
  at / try_at => At,
  colon / try_colon => Colon,
  dollar / try_dollar => Dollar,
  equal / try_equal => Equal,
  bang / try_bang => Exclamation,
  pipe / try_pipe => Pipe,
  ampersand / try_ampersand => Ampersand,
  lbrace / try_lbrace => OpenBrace,
  rbrace / try_rbrace => CloseBrace,
  lbracket / try_lbracket => OpenBracket,
  rbracket / try_rbracket => CloseBracket,
  lparen / try_lparen => OpenParen,
  rparen / try_rparen => CloseParen,
  spread / try_spread => Spread,
);

/// The identifier lexer `L` yields, over its source slice and span, tagged with
/// language `Lang` — the value both identifier atoms produce.
pub type IdentOf<'inp, L, Lang = ()> =
  tokora::types::Ident<SliceOf<'inp, L>, <L as Lexer<'inp>>::Span, Lang>;

/// Commits to an identifier; errors on anything else.
#[inline]
pub fn ident<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<IdentOf<'inp, L, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  tokora::types::Ident::parse_of(inp)
}

/// Declines (no tokens consumed) unless the next token is an identifier.
#[inline]
pub fn try_ident<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<IdentOf<'inp, L, Lang>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  tokora::types::Ident::try_parse_of(inp)
}

/// Returns `true` for the three spellings the spec excludes from `EnumValue`:
/// `true`, `false`, and `null`.
#[inline]
fn is_excluded_from_enum_value<S>(text: &S) -> bool
where
  S: Equivalent<str>,
{
  text.equivalent("true") || text.equivalent("false") || text.equivalent("null")
}

/// Commits to an `EnumValue`: a `Name` that is not `true`, `false`, or `null`.
/// The spec carves out exactly this one exclusion from `Name`, which is
/// otherwise unreserved, so soft keywords such as `enum`/`type` parse here
/// exactly as [`ident`] accepts them. The same exclusion governs the `Name`
/// that `EnumValueDefinition` introduces, so this atom backs both positions.
///
/// Errors on a non-identifier token, end of input, or an identifier spelled
/// `true`, `false`, or `null` — consuming whatever token is next either way,
/// exactly like [`ident`]'s commit discipline.
#[inline]
pub fn enum_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<IdentOf<'inp, L, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
  SliceOf<'inp, L>: Equivalent<str>,
{
  match inp.next()? {
    Some(spanned) => {
      if spanned.data().is_identifier() {
        let text = inp.slice();
        if is_excluded_from_enum_value(&text) {
          let (span, tok) = spanned.into_components();
          Err(UnexpectedToken::of(span).with_found(tok).into())
        } else {
          Ok(tokora::types::Ident::new(spanned.into_span(), text))
        }
      } else {
        let (span, tok) = spanned.into_components();
        Err(UnexpectedToken::of(span).with_found(tok).into())
      }
    }
    // Fully qualified so the `Span` trait need not join this module's imports.
    None => Err(UnexpectedEot::eot_of(tokora::Span::end(inp.span())).into()),
  }
}

/// Declines (no tokens consumed) unless the next token is an `EnumValue`: an
/// identifier that is not spelled `true`, `false`, or `null`. See
/// [`enum_value`] for the exclusion rule this enforces — `Name` but not those
/// three spellings, serving both the `EnumValue` production and
/// `EnumValueDefinition` — and [`try_ident`] for the declining discipline this
/// mirrors, including on the three excluded spellings, which decline exactly
/// as a non-identifier token would (nothing consumed either way).
#[inline]
pub fn try_enum_value<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
) -> Result<ParseAttempt<IdentOf<'inp, L, Lang>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: IdentifierToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
  SliceOf<'inp, L>: Equivalent<str>,
{
  let mut failed = None;
  let accepted = inp.attempt(|inp| match try_ident(inp) {
    Ok(ParseAttempt::Accept(id)) if !is_excluded_from_enum_value(id.source_ref()) => Some(id),
    Ok(_) => None,
    Err(err) => {
      failed = Some(err);
      None
    }
  });
  match failed {
    Some(err) => Err(err),
    None => Ok(match accepted {
      Some(id) => ParseAttempt::Accept(id),
      None => ParseAttempt::Decline,
    }),
  }
}

/// The keyword payload both keyword atoms produce: the token `L` yields for the
/// matched keyword, carried with its span and tagged with language `Lang`.
///
/// Unlike [`IdentOf`], the payload's source is the whole matched token rather
/// than a source slice, mirroring the non-sliced tokora keyword parser the atoms
/// delegate to.
pub type KeywordOf<'inp, L, Lang = ()> =
  tokora::types::Keyword<<L as Lexer<'inp>>::Token, <L as Lexer<'inp>>::Span, Lang>;

/// Commits to the keyword whose spelling equals `kw`; errors on any other token.
#[inline]
pub fn keyword_exact<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw: &'static str,
) -> Result<KeywordOf<'inp, L, Lang>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: KeywordToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>
    + From<UnexpectedToken<'inp, L::Token, <L::Token as Token<'inp>>::Kind, L::Span, Lang>>,
{
  tokora::types::Keyword::parse_exact_of(&kw).parse_input(inp)
}

/// Declines (no tokens consumed) unless the next token is the keyword whose
/// spelling equals `kw`.
#[inline]
pub fn try_keyword_exact<'inp, L, Ctx, Lang>(
  inp: &mut InputRef<'inp, '_, L, Ctx, Lang>,
  kw: &'static str,
) -> Result<ParseAttempt<KeywordOf<'inp, L, Lang>>, ErrorOf<'inp, L, Ctx, Lang>>
where
  L: Lexer<'inp>,
  L::Token: KeywordToken<'inp>,
  Ctx: ParseCtx<'inp, L, Lang>,
  Lang: ?Sized,
  ErrorOf<'inp, L, Ctx, Lang>: From<UnexpectedEot<L::Offset, Lang>>,
{
  tokora::types::Keyword::try_parse_exact_of(&kw).try_parse_input(inp)
}

/// Generates a committed/declining atom pair for one keyword spelling, mapping
/// the matched keyword onto a dialect's typed keyword node.
///
/// Each `name / try_name => "kw" => Ty` arm produces `name`, which commits to the
/// keyword spelled `"kw"` and returns `Ty` (erroring on anything else), and
/// `try_name`, which declines without consuming input unless `"kw"` is next. `Ty`
/// must expose a `new(SimpleSpan) -> Ty` constructor, which the atoms feed the
/// matched keyword's span; the generated atoms therefore require the lexer's span
/// to be [`SimpleSpan`](tokora::SimpleSpan).
///
/// The definition names no concrete keyword node, so a dialect assembly plugs in
/// its own `Ty` at the invocation site.
#[macro_export]
macro_rules! typed_keyword_atom {
  ($($fn_name:ident / $try_fn_name:ident => $kw:literal => $Ty:path),+ $(,)?) => {
    $(
      #[doc = ::core::concat!(
        "Commits to the `", $kw, "` keyword and returns its typed node; errors on anything else."
      )]
      #[inline]
      pub fn $fn_name<'inp, L, Ctx, Lang>(
        inp: &mut ::tokora::InputRef<'inp, '_, L, Ctx, Lang>,
      ) -> ::core::result::Result<$Ty, $crate::combinator::ErrorOf<'inp, L, Ctx, Lang>>
      where
        L: ::tokora::Lexer<'inp, Span = ::tokora::SimpleSpan>,
        L::Token: ::tokora::token::KeywordToken<'inp>,
        Ctx: $crate::combinator::ParseCtx<'inp, L, Lang>,
        Lang: ?::core::marker::Sized,
        $crate::combinator::ErrorOf<'inp, L, Ctx, Lang>:
          ::core::convert::From<::tokora::error::UnexpectedEot<L::Offset, Lang>>
            + ::core::convert::From<
              ::tokora::error::token::UnexpectedToken<
                'inp,
                L::Token,
                <L::Token as ::tokora::Token<'inp>>::Kind,
                L::Span,
                Lang,
              >,
            >,
      {
        $crate::combinator::keyword_exact(inp, $kw).map(|kw| <$Ty>::new(kw.span()))
      }

      #[doc = ::core::concat!(
        "Declines (no tokens consumed) unless the next token is the `", $kw,
        "` keyword, whose typed node it then returns."
      )]
      #[inline]
      pub fn $try_fn_name<'inp, L, Ctx, Lang>(
        inp: &mut ::tokora::InputRef<'inp, '_, L, Ctx, Lang>,
      ) -> ::core::result::Result<
        ::tokora::try_parse_input::ParseAttempt<$Ty>,
        $crate::combinator::ErrorOf<'inp, L, Ctx, Lang>,
      >
      where
        L: ::tokora::Lexer<'inp, Span = ::tokora::SimpleSpan>,
        L::Token: ::tokora::token::KeywordToken<'inp>,
        Ctx: $crate::combinator::ParseCtx<'inp, L, Lang>,
        Lang: ?::core::marker::Sized,
        $crate::combinator::ErrorOf<'inp, L, Ctx, Lang>:
          ::core::convert::From<::tokora::error::UnexpectedEot<L::Offset, Lang>>,
      {
        $crate::combinator::try_keyword_exact(inp, $kw)
          .map(|attempt| attempt.map(|kw| <$Ty>::new(kw.span())))
      }
    )+
  };
}

#[cfg(test)]
mod tests;
