use super::{BraceStop, error};
use tokora::{
  SimpleSpan,
  error::UnexpectedEnd,
  logos::{Lexer, Logos, Source},
  state::LimitExceeded,
  utils::Lexeme,
};

use crate::{
  graphqlx::{
    LitChar, LitPrefixedStr,
    error::{BinaryError, HexError, OctalError},
    lossless::LosslessState,
  },
  handlers::{self, is_ignored_char},
  hints::{BinaryHint, ExponentHint, FloatHint, HexExponentHint, HexFloatHint, HexHint, OctalHint},
};

type LexerError<Extras> = error::LexerError<char, Extras>;
type LexerErrors<Extras> = error::LexerErrors<char, Extras>;

#[inline(always)]
pub(crate) fn default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  match lexer.slice().as_ref().chars().next() {
    Some(ch) => LexerError::unknown_char(lexer.span().into(), ch, lexer.span().start),
    None => LexerError::unexpected_eoi(lexer.span().into()),
  }
  .into()
}

#[inline(always)]
pub(crate) fn cst_default_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> error::LexerErrors<char, Extras>
where
  T: Logos<'a, Source = S, Extras = LosslessState>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  handlers::cst_default_error(
    lexer,
    |lexer| lexer.slice().as_ref().chars().next(),
    LexerError::unknown_char,
    LexerError::unexpected_eoi,
  )
  .into()
}

/// The lossless `Comment` callback over a text source.
///
/// The rule's pattern matches the whole comment, `#` through the end of its body, before the first
/// `\n` or `\r` or at the end of input, and this callback only returns the slice. A text source is
/// UTF-8 by its type, so there is nothing to check and the answer is always `Ok`; the `Result` is
/// the shape the rule's hook shares with the byte source's callback. Mirrors the GraphQL callback
/// of the same name.
#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn comment<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Ok(lexer.slice())
}

#[inline(always)]
pub(crate) fn handle_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  // Inside a block the refusal ends before a brace or an angle bracket, as at the end of input.
  let ends = super::ends_before(lexer.extras.stops_before_a_brace(), is_ignored_char);
  Err(LexerError::float(
    lexer.span().into(),
    super::fractional_error(lexer, remainder_len, iter, ends),
  ))
}

#[inline(always)]
pub(crate) fn handle_hex_fractional_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  // Inside a block the refusal ends before a brace or an angle bracket, as at the end of input.
  let ends = super::ends_before(lexer.extras.stops_before_a_brace(), is_ignored_char);
  Err(LexerError::hex_float(
    lexer.span().into(),
    super::hex_fractional_error(lexer, remainder_len, iter, ends),
  ))
}

#[inline]
fn exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  // Inside a block the refusal ends before a brace or an angle bracket, as at the end of input.
  let ends = super::ends_before(lexer.extras.stops_before_a_brace(), is_ignored_char);

  let slice = lexer.slice();
  let slice_str = slice.as_ref();
  LexerError::float(
    lexer.span().into(),
    handlers::lit_float_suffix_error::<_, super::GraphQLxNumber, _, _, _, _>(
      "float",
      lexer,
      remainder_len,
      iter,
      ends,
      || match slice_str.chars().last() {
        Some('e' | 'E') => FloatHint::Exponent(ExponentHint::SignOrDigit),
        Some('+' | '-') => FloatHint::Exponent(ExponentHint::Digit),
        _ => unreachable!("regex should ensure the last char is 'e', 'E', '+' or '-"),
      },
    ),
  )
}

#[inline(always)]
pub(crate) fn handle_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(exponent_error(lexer))
}

#[inline]
fn hex_exponent_error<'a, S, T, Extras>(lexer: &mut Lexer<'a, T>) -> LexerError<Extras>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_str = remainder.as_ref();
  let remainder_len = remainder_str.len();
  let iter = remainder_str.chars();
  // Inside a block the refusal ends before a brace or an angle bracket, as at the end of input.
  let ends = super::ends_before(lexer.extras.stops_before_a_brace(), is_ignored_char);

  let slice = lexer.slice();
  let slice_str = slice.as_ref();

  LexerError::hex_float(
    lexer.span().into(),
    handlers::lit_float_suffix_error::<_, super::GraphQLxHexExponent, _, _, _, _>(
      "hex float",
      lexer,
      remainder_len,
      iter,
      ends,
      || match slice_str.chars().last() {
        Some('p' | 'P') => HexFloatHint::Exponent(HexExponentHint::SignOrDigit),
        Some('+' | '-' | '_') => HexFloatHint::Exponent(HexExponentHint::Digit),
        _ => unreachable!("regex should ensure the last char is 'p', 'P', '+', '-' or '_'"),
      },
    ),
  )
}

#[inline(always)]
pub(crate) fn handle_hex_exponent_error<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S, Extras: BraceStop>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  Err(hex_exponent_error(lexer))
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_float_missing_integer_part_error_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_hex_float_missing_integer_part_error_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_integer_part_error_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

#[allow(clippy::result_large_err)]
#[inline]
pub(crate) fn handle_hex_float_missing_exponent_then_check_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_missing_exponent_then_check_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
  )
}

pub(crate) fn handle_decimal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_decimal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

pub(crate) fn handle_valid_binary_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_binary_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[inline]
pub(crate) fn handle_invalid_binary_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = error::LexerErrors::default();
  let no_digit = error::LexerError::binary(
    lexer.span().into(),
    BinaryError::UnexpectedEnd(UnexpectedEnd::with_name(
      0,
      "binary".into(),
      BinaryHint::Digit,
    )),
  );
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(no_digit);
    return Err(errs);
  }

  match handle_valid_binary_suffix(lexer, BinaryError::UnexpectedSuffix) {
    Ok(_) => {
      errs.push(no_digit);
      Err(errs)
    }
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_octal_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_octal_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[inline]
pub(crate) fn handle_invalid_octal_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = error::LexerErrors::default();
  let no_digit = error::LexerError::octal(
    lexer.span().into(),
    OctalError::UnexpectedEnd(UnexpectedEnd::with_name(
      0,
      "octal".into(),
      OctalHint::Digit,
    )),
  );
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(no_digit);
    return Err(errs);
  }

  match handle_valid_octal_suffix(lexer, OctalError::UnexpectedSuffix) {
    Ok(_) => {
      errs.push(no_digit);
      Err(errs)
    }
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

pub(crate) fn handle_valid_hex_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_valid_hex_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

#[inline]
pub(crate) fn handle_invalid_hex_suffix<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<S::Slice<'a>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  let mut errs = error::LexerErrors::default();
  let no_digit = error::LexerError::hex(
    lexer.span().into(),
    HexError::UnexpectedEnd(UnexpectedEnd::with_name(0, "hex".into(), HexHint::Digit)),
  );
  let remainder = lexer.remainder();
  let remainder_ref = remainder.as_ref();

  if remainder_ref.is_empty() {
    errs.push(no_digit);
    return Err(errs);
  }

  match handle_valid_hex_suffix(lexer, HexError::UnexpectedSuffix) {
    Ok(_) => {
      errs.push(no_digit);
      Err(errs)
    }
    Err(e) => {
      errs.push(error::LexerError::new(lexer.span(), e.into()));
      Err(errs)
    }
  }
}

/// The valid hexadecimal float's check of what follows its exponent, over a text source.
///
/// The exponent is decimal, so a letter after it is no digit of it: `0x1p1f` is refused at the
/// `f`, where the class of hexadecimal digits would let `0x1p1` stand and `f` begin a name.
pub(crate) fn handle_valid_hex_float_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let remainder = lexer.remainder();
  let remainder_len = remainder.as_ref().len();
  super::handle_hex_float_suffix(
    lexer,
    remainder_len,
    remainder.as_ref().chars(),
    unexpected_suffix,
  )
  .map_err(|e| LexerError::new(lexer.span(), e.into()))
}

/// A number with an integer suffix over a text source, as the rule matched it; or, when the
/// suffix does not stand, the number refused as a letter after it is.
pub(crate) fn handle_int_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  typed_suffix(lexer, super::Suffix::Int, unexpected_suffix)
}

/// A number with a float suffix over a text source, as the rule matched it; or, when the suffix
/// does not stand, the number refused as a letter after it is.
pub(crate) fn handle_float_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  typed_suffix(lexer, super::Suffix::Float, unexpected_suffix)
}

/// The two suffix handlers' one body: the match as it stands, or the refused run bumped over and
/// reported as `unexpected_suffix` reports a letter after the number.
fn typed_suffix<'a, S, T, E, Extras>(
  lexer: &mut Lexer<'a, T>,
  suffix: super::Suffix,
  unexpected_suffix: impl FnOnce(Lexeme<char>) -> E,
) -> Result<S::Slice<'a>, LexerError<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
  E: Into<error::LexerErrorData<char, Extras>>,
{
  let slice = lexer.slice();
  let remainder = lexer.remainder();
  match super::suffix_refusal(
    slice.as_ref().as_bytes(),
    remainder.as_ref().as_bytes(),
    suffix,
  ) {
    None => Ok(slice),
    Some(super::SuffixRefusal { letter, past }) => {
      let at = lexer.span().start.saturating_add(letter);
      lexer.bump(past);
      let lexeme = Lexeme::Range(SimpleSpan::new(at, lexer.span().end));
      Err(LexerError::new(
        lexer.span(),
        unexpected_suffix(lexeme).into(),
      ))
    }
  }
}

/// The lossless `'` rule's callback over a text source: the `char` literal the `'` opens, read to
/// its end by the reader both of the dialect's lexers share, and bumped over.
#[inline]
pub(crate) fn char_literal<'a, S, T, Extras>(
  lexer: &mut Lexer<'a, T>,
) -> Result<LitChar<S::Slice<'a>>, LexerErrors<Extras>>
where
  T: Logos<'a, Source = S>,
  S: ?Sized + Source,
  S::Slice<'a>: AsRef<str>,
{
  use crate::string_lexer::char_literal::{CharBody, CharLiteral};

  let start = lexer.span().start;
  let rest = lexer.remainder();
  let text: &str = rest.as_ref();
  let CharLiteral { len, read } = <char as CharBody>::read_char_literal(text.as_bytes(), start);
  // The rule took the opening `'`, which `len` counts.
  lexer.bump(len - 1);
  match read {
    Ok(value) => Ok(LitChar::new(lexer.slice(), value)),
    Err(refusal) => Err(error::char_refusal(refusal, lexer.span().into())),
  }
}

/// The lossless name rule's callback inside a `const { … }` block, when a quote touches the name,
/// over a text source: one lexeme over the name and the literal after it. `b` or `i` before a
/// string makes a prefixed string; any other name before a string, and any name before a `char`
/// literal, is the reserved prefix's refusal, whose record holds the literal's own errors after its
/// own. The literal is read first, so the lexeme's one charge is checked at its whole extent. Kept
/// out of line: the name rule is the grammar's commonest, and this is the block's alone.
#[allow(clippy::result_large_err)]
#[inline(never)]
pub(crate) fn prefixed<'a, S, T>(
  lexer: &mut Lexer<'a, T>,
) -> Result<LitPrefixedStr<S::Slice<'a>>, LexerErrors<LimitExceeded>>
where
  T: Logos<'a, Source = S, Extras = LosslessState>,
  S: ?Sized + Source + 'a,
  S::Slice<'a>: AsRef<str>,
{
  use crate::{
    LitBlockStr, LitInlineStr, SealedWrapper,
    error::StringErrors,
    graphqlx::{StrPrefix, error::TouchedLiteral},
    string_lexer::{
      byte_str::{ByteStr, ByteStrBody},
      char_literal::{CharBody, CharLiteral},
    },
  };
  use tokora::lexer::Lexable;

  let quote = lexer.span().end;
  let name = lexer.slice();
  let prefix = match AsRef::<str>::as_ref(&name) {
    "b" => Some(StrPrefix::Bytes),
    "i" => Some(StrPrefix::Id),
    _ => None,
  };
  let rest = lexer.remainder();
  let after: &[u8] = AsRef::<str>::as_ref(&rest).as_bytes();
  let char_literal = after.first() == Some(&b'\'');
  let block = !char_literal && after.starts_with(b"\"\"\"");
  let body: &[u8] = after.get(1..).unwrap_or_default();

  // The literal, read to its end, and its refusal when it is refused.
  let refused = if char_literal {
    let CharLiteral { len, read } = <char as CharBody>::read_char_literal(body, quote);
    lexer.bump(len);
    read.err().map(TouchedLiteral::Char)
  } else if prefix == Some(StrPrefix::Bytes) && !block {
    let ByteStr { len, read } = <char as ByteStrBody>::read_byte_str(body, quote);
    lexer.bump(len);
    read.err().map(TouchedLiteral::String)
  } else if block {
    lexer.bump(3);
    <LitBlockStr<S::Slice<'a>> as Lexable<_, StringErrors<char>>>::lex(
      SealedWrapper::<Lexer<'a, T>>::from_mut(lexer),
    )
    .err()
    .map(TouchedLiteral::String)
  } else {
    lexer.bump(1);
    <LitInlineStr<S::Slice<'a>> as Lexable<_, StringErrors<char>>>::lex(
      SealedWrapper::<Lexer<'a, T>>::from_mut(lexer),
    )
    .err()
    .map(TouchedLiteral::String)
  };
  let end = lexer.span().end;

  super::tt_hook_and_then_into_errors(lexer, |lexer: &mut Lexer<'a, T>| {
    match (prefix, char_literal, refused) {
      (Some(prefix), false, None) => match lexer.source().slice(quote..end) {
        Some(slice) => Ok(LitPrefixedStr::new(prefix, block, slice)),
        // Not reached: both ends are positions this lexer stood on. Refused all the same.
        None => Err(error::prefixed_refusal(lexer.span().into(), true, None)),
      },
      (prefix, char_literal, refused) => Err(error::prefixed_refusal(
        lexer.span().into(),
        prefix.is_none() || char_literal,
        refused,
      )),
    }
  })
}
