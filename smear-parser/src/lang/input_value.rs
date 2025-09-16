use chumsky::{container::Container, extra::ParserExtra, prelude::*};
use either::Either;

use super::{
  super::source::*,
  ignored,
  punct::{Colon, Equal, LAngle},
};

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
pub use tuple::*;
pub use uint::*;
pub use variable::*;

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
mod tuple;
mod uint;
mod variable;

/// A GraphQL default value assignment for input parameters.
///
/// Represents the default value assignment syntax used in GraphQL variable
/// declarations, field arguments, and input type definitions. Default values
/// provide fallback values when no explicit value is provided, following
/// GraphQL's default value semantics and constant expression requirements.
///
/// ## Specification Rules
///
/// GraphQL default values follow strict formatting and semantic rules:
/// - **Equals syntax**: Must use `=` to assign the default value
/// - **Constant requirement**: Default values must be constant expressions (no variables)
/// - **Type compatibility**: Default value type must match the declared type
/// - **Nullability handling**: Non-null types can have null defaults (making them effectively nullable)
/// - **Whitespace flexibility**: Optional whitespace around the `=` token
///
/// ## Grammar
///
/// ```text
/// DefaultValue ::= '=' Value
/// ```
#[derive(Debug, Clone, Copy)]
pub struct DefaultInputValue<Value, Span> {
  span: Span,
  eq: Equal<Span>,
  value: Value,
}

impl<Value, Span> DefaultInputValue<Value, Span> {
  /// Returns the source span of the entire default value assignment.
  ///
  /// This span covers from the `=` token through the last character of the
  /// default value, providing the complete source location for error reporting
  /// and source mapping.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the equals assignment token.
  ///
  /// This provides access to the `=` character that introduces the default
  /// value assignment, including its exact source position. Useful for syntax
  /// highlighting and precise error reporting.
  #[inline]
  pub const fn eq(&self) -> &Equal<Span> {
    &self.eq
  }

  /// Returns the default value expression.
  ///
  /// This provides access to the constant expression that serves as the
  /// default value.
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// Creates a parser for default value assignments with constant validation.
  ///
  /// This parser handles the complete default value syntax including the equals
  /// token, optional whitespace, and the default value expression. It enforces
  /// GraphQL's requirement that default values must be constant expressions
  /// through compile-time type constraints.
  ///
  ///
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src>,
    I::Token: Char + 'src,
    I::Slice: Slice<Token = I::Token>,
    E: ParserExtra<'src, I>,
    Span: crate::source::FromMapExtra<'src, I, E>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    Equal::parser()
      .then_ignore(ignored::ignored())
      .then(value)
      .map_with(|(eq, value), sp| Self {
        span: Span::from_map_extra(sp),
        eq,
        value,
      })
  }
}

impl<Value, Span> AsRef<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Value, Span> IntoSpan<Span> for DefaultInputValue<Value, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Value, Span> IntoComponents for DefaultInputValue<Value, Span> {
  type Components = (Span, Equal<Span>, Value);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.eq, self.value)
  }
}

/// Returns a parser which can parse either a set or a map.
pub fn map_or_set_parser<'src, I, E, Key, Value, Item, KP, VP, IP, CS, CM>(
  key_parser: impl Fn() -> KP,
  value_parser: impl Fn() -> VP,
  item_parser: impl Fn() -> IP,
) -> impl Parser<'src, I, Either<Set<Item, I::Span, CS>, Map<Key, Value, I::Span, CM>>, E> + Clone
where
  I: Source<'src>,
  I::Token: Char + 'src,
  I::Slice: Slice<Token = I::Token>,
  I::Span: crate::source::FromMapExtra<'src, I, E>,
  E: ParserExtra<'src, I>,
  KP: Parser<'src, I, Key, E> + Clone,
  VP: Parser<'src, I, Value, E> + Clone,
  IP: Parser<'src, I, Item, E> + Clone,
  CM: Container<MapEntry<Key, Value, I::Span>>,
  CS: Container<Item>,
{
  // Non-consuming guard:
  // after '<' and ws, either a ':' (i.e. "<:>") or "value ':'" => it's a Map.
  let map_guard = LAngle::<I::Span>::parser()
    .ignore_then(ignored())
    .ignore_then(choice((
      Colon::<I::Span>::parser().to(()), // "<:>"
      key_parser()
        .then_ignore(ignored())
        .then_ignore(Colon::<I::Span>::parser())
        .to(()), // "< value :"
    )))
    .rewind();

  map_guard
    .ignore_then(Map::<Key, Value, _, CM>::parser_with(
      key_parser(),
      value_parser(),
    ))
    .map(Either::Right)
    .or(Set::<Item, _, CS>::parser_with(item_parser()).map(Either::Left))
}
