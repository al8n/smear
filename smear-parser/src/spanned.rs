use chumsky::{extra::ParserExtra, input::SliceInput};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Spanned<Src, Span> {
  src: Src,
  span: Span,
}

impl<Src, Span> Spanned<Src, Span> {
  /// Create a new `Spanned` value.
  pub const fn new(src: Src, span: Span) -> Self {
    Self { src, span }
  }

  /// Returns the source
  pub const fn source(&self) -> &Src {
    &self.src
  }

  /// Returns the span
  pub const fn span(&self) -> &Span {
    &self.span
  }
}

impl<'src, 'b, I: SliceInput<'src>, E: ParserExtra<'src, I>>
  From<&mut chumsky::input::MapExtra<'src, 'b, I, E>> for Spanned<I::Slice, I::Span>
{
  fn from(value: &mut chumsky::input::MapExtra<'src, 'b, I, E>) -> Self {
    Self::new(value.slice(), value.span())
  }
}
