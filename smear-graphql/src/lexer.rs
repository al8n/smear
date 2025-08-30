use chumsky::{label::LabelError, prelude::*};
use smear_parser::lexer::{self, DisplayText, Lexer, LexerError, Positioned, Span, State, Text};

/// The kind of the [`Token`]
#[derive(Debug, PartialEq, Eq, Clone, Copy, derive_more::Display)]
pub enum TokenKind {
  /// The punctuation token.
  Punctuator,
  /// The spread token. '...'
  Spread,
  /// The name token.
  Name,
  /// The integer value token.
  IntValue,
  /// The float value token.
  FloatValue,
  /// The string value token.
  StringValue,
  /// The block string token.
  BlockString,

  /// The ignored tokens.
  Ignored,
  /// The EOF token
  End,
}

impl lexer::TokenKind for TokenKind {
  #[inline(always)]
  fn comment() -> Self {
    Self::Ignored
  }

  #[inline(always)]
  fn line_terminator() -> Self {
    Self::Ignored
  }

  #[inline(always)]
  fn whitespace() -> Self {
    Self::Ignored
  }

  fn ignored() -> Self {
    Self::Ignored
  }

  fn name() -> Self {
    Self::Name
  }
}

#[derive(Debug, Clone)]
pub enum TokenErrorData<I> {
  /// Unknown token for the GraphQL lexer
  Unknown {
    /// The invalid data
    data: I,
    /// The global position of the invalid data to the original input.
    position: usize,
  },
}

impl<I> TokenErrorData<I> {
  /// Creates a unknown error data
  #[inline]
  pub const fn unknown(data: I, position: usize) -> Self {
    Self::Unknown { data, position }
  }
}

impl<'a, I: Text<'a>> core::fmt::Display for TokenErrorData<I> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    match self {
      Self::Unknown { data, position } => {
        write!(
          f,
          "Unknown token '{}' at position {}",
          DisplayText::from(data),
          position
        )
      }
    }
  }
}

impl<'a, I: Text<'a> + core::fmt::Debug> core::error::Error for TokenErrorData<I> {}

#[derive(Clone, Debug)]
pub struct TokenError<I, S> {
  data: TokenErrorData<I>,
  span: Span<S>,
}

impl<'a, I, S> TokenError<I, S> {
  #[inline]
  const fn new(data: TokenErrorData<I>, span: Span<S>) -> Self {
    Self { data, span }
  }

  /// Creates a unknown error
  #[inline]
  pub const fn unknown(data: I, position: usize, span: Span<S>) -> Self {
    Self::new(TokenErrorData::unknown(data, position), span)
  }
}

impl<'a, I, S> lexer::TokenError<S> for TokenError<I, S>
where
  I: Text<'a> + core::fmt::Debug,
  S: State + core::fmt::Debug,
{
  #[inline(always)]
  fn span(&self) -> Span<S> {
    self.span
  }
}

impl<'a, I: Text<'a>, S: State> core::fmt::Display for TokenError<I, S> {
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.data.fmt(f)
  }
}

impl<'a, I: Text<'a> + core::fmt::Debug, S: State + core::fmt::Debug> core::error::Error
  for TokenError<I, S>
{
}

#[derive(Debug, Clone, Copy)]
pub struct Token<I, S> {
  kind: TokenKind,
  data: I,
  span: Span<S>,
}

impl<'a, I, S> core::fmt::Display for Token<I, S>
where
  I: Text<'a>,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayText::from(&self.data).fmt(f)
  }
}

impl<I, S> Token<I, S> {
  #[inline(always)]
  const fn new(kind: TokenKind, data: I, span: Span<S>) -> Self {
    Self { kind, data, span }
  }
}

impl<'a, S> lexer::Token<'a, &'a str, S> for Token<&'a str, S>
where
  S: State + 'a,
{
  type Error = TokenError<&'a str, S>;
  type Kind = TokenKind;

  fn peek(input: Positioned<&'a str>, state: &mut S) -> Result<Option<(usize, Self)>, Self::Error> {
    // let bytes = input.data();
    // match Token::<&'a [u8]>::peek(bytes, state) {
    //   Ok(Some((consumed, tok))) => {
    //     *cursor += consumed;
    //     Some(Ok(Some(tok)))
    //   }
    //   Ok(None) => None,
    //   Err(e) => Some(Err(LexerError::Token(e))),
    // }
    todo!()
  }

  #[inline(always)]
  fn kind(&self) -> TokenKind {
    self.kind
  }

  #[inline(always)]
  fn data(&self) -> &'a str {
    self.data
  }

  #[inline(always)]
  fn span(&self) -> Span<S> {
    self.span
  }
}

impl<'a, S> lexer::Token<'a, &'a [u8], S> for Token<&'a [u8], S>
where
  S: State + 'a,
{
  type Error = TokenError<&'a [u8], S>;
  type Kind = TokenKind;

  fn peek(
    input: Positioned<&'a [u8]>,
    state: &mut S,
  ) -> Result<Option<(usize, Self)>, Self::Error> {
    let data = input.data();
    let mut bytes = data.iter().enumerate();

    let (mut idx, cur) = match bytes.next() {
      Some((i, &b)) => (i, b),
      None => {
        return Ok(None);
      }
    };

    let skipped = skip_ignored_tokens(data, state);
    if skipped > 0 {
      return Ok(Some((
        skipped,
        Self::new(
          TokenKind::Ignored,
          &data[..skipped],
          Span::new(input.global_position(), input.global_position() + skipped, *state),
        ),
      )));
    }

    match cur {
      b'[' | b'(' | b'{' => {
        state.increase_recursion();
        let span = Span::new(idx, idx + 1, *state);
        Ok(Some((
          1,
          Self::new(TokenKind::Punctuator, &data[idx..idx + 1], span),
        )))
      }
      b']' | b')' | b'}' => {
        state.decrease_recursion();
        let span = Span::new(idx, idx + 1, *state);
        Ok(Some((
          1,
          Self::new(TokenKind::Punctuator, &data[idx..idx + 1], span),
        )))
      }
      b'!' | b'$' | b':' | b'=' | b'@' | b'|' | b'&' => {
        state.increase_token();
        let span = Span::new(idx, idx + 1, *state);
        Ok(Some((
          1,
          Self::new(TokenKind::Punctuator, &data[idx..idx + 1], span),
        )))
      }
      b'.' => {
        let end = idx + 3;
        if data.get(idx..end) == Some(b"...") {
          let span = Span::new(idx, end, *state);
          Ok(Some((
            3,
            Self::new(TokenKind::Spread, &data[idx..end], span),
          )))
        } else {
          let span = Span::new(idx, idx + 1, *state);
          Err(TokenError::unknown(
            &data[idx..idx + 1],
            input.global_position() + idx,
            span,
          ))
        }
      }
      b'_' | b'a'..=b'z' | b'A'..=b'Z' => {
        for (_, &b) in bytes {
          match b {
            b'_' | b'a'..=b'z' | b'A'..=b'Z' | b'0'..=b'9' => {
              idx += 1;
              continue;
            },
            _ => {
              break;
            },
          }
        }

        state.increase_column_number(idx);
        let name_end = idx + 1;
        Ok(Some((
          name_end,
          Self::new(
            TokenKind::Name,
            &data[..name_end],
            Span::new(input.global_position(), input.global_position() + name_end, *state),
          ),
        )))
      },
      other => {
        println!("other: {}", other);
        let span = Span::new(idx, idx + 1, *state);
        Err(TokenError::unknown(
          &data[idx..idx + 1],
          input.global_position() + idx,
          span,
        ))
      }
    }
  }

  #[inline(always)]
  fn kind(&self) -> TokenKind {
    self.kind
  }

  #[inline(always)]
  fn data(&self) -> &'a [u8] {
    self.data
  }

  #[inline(always)]
  fn span(&self) -> Span<S> {
    self.span
  }
}

struct Skip {
  lines: usize,
  columns: usize,
  // total number of bytes skipped
  num: usize,
}

fn skip_ignored_tokens<S>(input: &[u8], state: &mut S) -> usize
where
  S: State,
{
  let tab_width = state.tab_width();
  let mut idx = 0;
  let end = input.len();

  'outer: while idx < end {
    match input[idx] {
      b' ' | b',' => {
        state.increase_column_number(1);
        idx += 1;
      }
      b'\t' => {
        state.increase_column_number(tab_width);
        idx += 1;
      }
      b'\n' => {
        state.increase_line_number(1);
        idx += 1;
      }
      b'\r' => {
        // According to the spec, if a `\r` is followed by a `\n`, `\r\n` should be treated as a line break.
        // if a `\r` is not followed by a `\n`, the `\r` itself should be treated as a line break.
        if let Some(&b'\n') = input.get(idx + 1) {
          state.increase_line_number(1);
          idx += 2;
        } else {
          state.increase_line_number(1);
          idx += 1;
        }
      }
      b'#' => { 
        idx += 1;
        state.increase_column_number(1);

        let comment_start = idx;
        while idx < end {
          match input[idx] {
            b'\n' => {
              state.increase_line_number(1);
              idx += 1;
              continue 'outer;
            }
            b'\r' => {
              // According to the spec, if a `\r` is followed by a `\n`, `\r\n` should be treated as a line break.
              // if a `\r` is not followed by a `\n`, the `\r` itself should be treated as a line break.
              if let Some(&b'\n') = input.get(idx + 1) {
                state.increase_line_number(1);
                idx += 2;
              } else {
                state.increase_line_number(1);
                idx += 1;
              }
              continue 'outer;
            }
            _ => {
              idx += 1;
            }
          }
        }
        state.increase_column_number(idx - comment_start);
      },
      b'\xEF' => {
        // BOM (Byte Order Mark): \u{FEFF} == [b'\xEF', b'\xBB', b'\xBF']
        if let Some([b'\xBB', b'\xBF']) = input.get(idx..idx + 2) {
          state.increase_column_number(3);
          idx += 3;
        } else {
          state.increase_column_number(1);
          idx += 1;
        }
      }
      _ => break,
    }
  }

  idx
}

// /// Returns a parser that produces a stream of tokens.
// pub fn lexer<'a, I, S, C, E>() -> impl Parser<'a, Lexer<'a, I, Token<I>, S>, C, E>
// where
//   S: State,
//   I: Text<'a>,
//   Lexer<'a, I, Token<I>, S>: Input<'a>,
//   Token<I>: smear_parser::lexer::Token<'a, I, S>,
//   E: extra::ParserExtra<'a, Lexer<'a, I, Token<I>, S>>,
//   // E::Error: LabelError<'a, Lexer<'a, I, Token<I>, S>, LexerError<TokenError<I>, S::Error>>,
//   C: chumsky::container::Container<Result<Token<I>, LexerError<TokenError<I>, S::Error>>>,
// {
//   any()
//     .map(|tok| {
//       match tok {
//         Ok(Some(tok)) => Ok(tok),
//         Ok(None) => Err(LexerError::EndOfInput),
//         Err(e) => Err(e),
//       }
//     })
//     .repeated()
//     .collect::<C>()
// }

#[cfg(test)]
mod tests {
  use core::convert::Infallible;

  use smear_parser::{lang::{comment, comment1, comment2, ignored1, name}, lexer::Lexer};

  use super::*;

  #[test]
  fn t() {
    let input = r#" what
    
    
    
    
    # asdasdasdas
    ,,,,
    "#;

    let parser = name::<&[u8], Token<&[u8], ()>, (), extra::Err<EmptyErr>>().padded_by(ignored1());
    let res = parser
      .parse(Lexer::<&[u8], Token<&[u8], ()>, ()>::new(input.as_bytes()))
      .into_result()
      .unwrap();
    println!("{}", res);

    // println!("input: {}n", core::str::from_utf8(&input.as_bytes()[14..15]).unwrap());
    // let parser = comment::<&str, extra::Err<Rich<char>>>();
    // let res = parser
    //   .parse(input)
    //   .into_result()
    //   .unwrap();
    // println!("{:#?}", res);

    // let parser = any::<
    //   Lexer<'_, &str, Token<&str>, ()>,
    //   extra::Err<Simple<_, <Lexer<'_, &str, Token<&str>, ()> as Input<'_>>::Span>>,
    // >()
    // .try_map(|tok, sp| match tok {
    //   Ok(Some(tok)) => Ok(tok),
    //   Ok(None) => Err(LexerError::EndOfInput),
    //   Err(e) => Err(e),
    // })
    // .repeated()
    // .collect::<Vec<_>>();

    // let res: Result<
    //   Vec<Result<Token<&str>, LexerError<TokenError<&str>, Infallible>>>,
    //   Vec<
    //     Simple<
    //       '_,
    //       Result<Option<Token<&str>>, LexerError<TokenError<&str>, Infallible>>,
    //       SimpleSpan<usize, Context<&str, ()>>,
    //     >,
    //   >,
    // > = parser
    //   .parse(Lexer::<&str, Token<&str>>::new(input))
    //   .into_result();
    // let lexer = lexer::<&str, (), Vec<_>, extra::Err<LexerError<Token<&str>, Infallible>>>();
    // let tokens = lexer.parse(Lexer::<&str, Token<&str>>::new(input));
  }
}
