use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::{
  super::{char::Char, language::punct::Equal, spanned::Spanned},
  ignored,
};

/// Groto input value and const input value parsers
pub mod groto;

pub use angle::*;
pub use boolean::*;
pub use enum_::*;
pub use float::*;
pub use int::*;
pub use list::*;
pub use map::*;
pub use null::*;
pub use object::*;
pub use set::*;
pub use string::*;
pub use variable::*;

mod angle;
mod boolean;
mod enum_;
mod float;
mod int;
mod list;
mod map;
mod null;
mod object;
mod set;
mod string;
mod variable;

/// Default input value
#[derive(Debug, Clone)]
pub struct DefaultInputValue<Value, Src, Span> {
  span: Spanned<Src, Span>,
  eq: Equal<Src, Span>,
  value: Value,
}

impl<Value, Src, Span> DefaultInputValue<Value, Src, Span> {
  /// Returns the span of the default input value
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns a reference to the equal token
  #[inline]
  pub const fn eq(&self) -> &Equal<Src, Span> {
    &self.eq
  }

  /// Returns a reference to the value of the default input value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Returns a parser of default input value.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    Equal::parser()
      .then_ignore(ignored::ignored())
      .then(value)
      .map_with(|(eq, value), sp| Self {
        span: Spanned::from(sp),
        eq,
        value,
      })
      .then_ignore(ignored::ignored())
  }
}
