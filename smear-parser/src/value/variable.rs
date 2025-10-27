use core::fmt::Display;

use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, Parser, extra::ParserExtra},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};
use smear_lexer::punctuator::Dollar;

use crate::{error::ParseVariableValueError, hints::VariableValueHint};

/// A variable value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableValue<Name> {
  span: Span,
  name: Name,
}

impl<Name> AsSpan<Span> for VariableValue<Name> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name> IntoSpan<Span> for VariableValue<Name> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name> IntoComponents for VariableValue<Name> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

impl<Name> core::ops::Deref for VariableValue<Name> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name> Display for VariableValue<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "${}", self.name.display())
  }
}

impl<Name> VariableValue<Name> {
  /// Creates a new variable from the given span and name.
  #[cfg_attr(not(tarpaulin), inline(always))]
  pub(crate) const fn new(span: Span, name: Name) -> Self {
    Self { span, name }
  }

  /// Returns the span of the name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the name as a string slice.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name> DisplayCompact for VariableValue<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<Name> DisplayPretty for VariableValue<Name>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for VariableValue<Name>
where
  Error: ParseVariableValueError<Name>,
  Name: Parseable<'a, I, T, Error>,
  Dollar: Parseable<'a, I, T, Error>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, I, Self, E> + Clone
  where
    Self: Sized + 'a,
    E: ParserExtra<'a, I, Error = Error> + 'a,
    T: Token<'a>,
    I: Tokenizer<'a, T, Slice = <<T::Logos as Logos<'a>>::Source as Source>::Slice<'a>>,
    Error: 'a,
  {
    Dollar::parser()
      .or_not()
      .then(Name::parser().or_not())
      .try_map_with(|(dollar, name), exa| {
        let span = exa.span();
        match (dollar, name) {
          (None, None) => Err(Error::unexpected_end_of_variable_value(
            VariableValueHint::Dollar,
            span,
          )),
          (Some(_), None) => Err(Error::unexpected_end_of_variable_value(
            VariableValueHint::Name,
            span,
          )),
          (None, Some(name)) => Err(Error::missing_dollar_token(name, span)),
          (Some(_), Some(name)) => Ok(VariableValue::new(span, name)),
        }
      })
  }
}
