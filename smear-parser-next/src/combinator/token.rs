//! Token atoms: committed and declining parsers for GraphQL punctuators and for
//! identifiers.
//!
//! Each atom is a thin wrapper over the matching tokora parser. The committed form
//! (e.g. [`at`], [`ident`]) consumes the token and errors on anything else; the
//! declining form (e.g. [`try_at`], [`try_ident`]) consumes it only when it is
//! next and otherwise declines without touching the input.

use tokora::{
  InputRef, Lexer, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  token::{IdentifierToken, PunctuatorToken},
  try_parse_input::ParseAttempt,
};

use super::{ErrorOf, ParseCtx, SliceOf};

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
fn is_excluded_from_enum_value(text: &[u8]) -> bool {
  matches!(text, b"true" | b"false" | b"null")
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
  SliceOf<'inp, L>: AsRef<[u8]>,
{
  match inp.next()? {
    Some(spanned) => {
      if spanned.data().is_identifier() {
        let text = inp.slice();
        if is_excluded_from_enum_value(text.as_ref()) {
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
  SliceOf<'inp, L>: AsRef<[u8]>,
{
  let mut failed = None;
  let accepted = inp.attempt(|inp| match try_ident(inp) {
    Ok(ParseAttempt::Accept(id)) if !is_excluded_from_enum_value(id.source_ref().as_ref()) => {
      Some(id)
    }
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

#[cfg(test)]
mod tests;
