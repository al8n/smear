use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use crate::{char::Char, name::Name, source::Source, spanned::Spanned};

/// A parsed GraphQL **Enum Value**.
///
/// Per the spec, an enum value is any [`Name`] **except** the exact
/// lowercase keywords `true`, `false`, or `null`.
///
/// This parser enforces only the lexical rule above. It does **not**
/// check that the value is a member of a particular enum type; perform
/// that semantic validation at a higher layer.
///
/// - Case-sensitive: `FALSE` / `True` are allowed, but `false` / `true` are not.
/// - `trueValue`, `nullish`, etc. are allowed (only the exact keywords are banned).
/// - Surrounding whitespace/comments are not handled here; compose at a higher level.
///
/// Spec: <https://spec.graphql.org/draft/#sec-Enum-Value>
#[derive(Debug, Clone, Copy)]
pub struct EnumValue<Src, Span> {
  /// The name of the enum value
  name: Name<Src, Span>,
}

impl<Src, Span> EnumValue<Src, Span> {
  /// Returns the span of the enum value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }

  /// Returns the name of the enum value.
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }

  /// Returns a parser for the enum value.
  ///
  /// Spec: [Enum Value](https://spec.graphql.org/draft/#sec-Enum-Value)
  pub fn parser<'src, I, E>() -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
  {
    Name::<Src, Span>::parser()
      .filter(|name| {
        let src = name.span().source();
        !(I::is_true_slice(src) || I::is_false_slice(src) || I::is_null_slice(src))
      })
      .map(|name| Self { name })
      .labelled("enum value")
  }
}
