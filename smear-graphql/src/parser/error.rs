use logosky::{Token, utils::Span};


pub enum ErrorData<'a, T: Token<'a>> {
  UnexpectedToken {
    found: T,
    expected: T::Kind,
  },
}

pub struct Error<'a, T: Token<'a>> {
  span: Span,
  data: ErrorData<'a, T>,
}

impl<'a, T: Token<'a>> Error<'a, T> {
  /// Creates an unexpected token error.
  #[inline]
  pub const fn unexpected_token(
    found: T,
    expected: T::Kind,
    span: Span,
  ) -> Self {
    Self {
      span,
      data: ErrorData::UnexpectedToken { found, expected },
    }
  }
}


