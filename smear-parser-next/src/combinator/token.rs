//! Punctuator atoms: a committed and a declining parser for each GraphQL punctuator.
//!
//! Each atom is a thin wrapper over the matching [`tokora::punct`] parser. The
//! committed form (e.g. [`at`]) consumes the punctuator and errors on anything
//! else; the declining form (e.g. [`try_at`]) consumes it only when it is next
//! and otherwise declines without touching the input.

use tokora::{
  InputRef, Lexer, Token,
  error::{UnexpectedEot, token::UnexpectedToken},
  token::PunctuatorToken,
  try_parse_input::ParseAttempt,
};

use super::{ErrorOf, ParseCtx};

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

#[cfg(test)]
mod tests;
