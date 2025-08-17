use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use either::Either;

use crate::parser::{
  punct::{LAngle, RAngle},
  SmearChar, Spanned,
};

#[derive(Debug, Clone)]
pub struct MapEntry<T, Src, Span> {
  span: Spanned<Src, Span>,
  colon: Spanned<Src, Span>,
  key: T,
  value: T,
}

impl<T, Src, Span> MapEntry<T, Src, Span> {
  /// Returns the span of the entry.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the span of the colon
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }

  /// Returns the key of the entry.
  #[inline]
  pub const fn key(&self) -> &T {
    &self.key
  }

  /// Returns the value of the entry.
  #[inline]
  pub const fn value(&self) -> &T {
    &self.value
  }

  /// Returns a parser for the field.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    value
      .clone()
      .then(
        just(I::Token::COLON)
          .map_with(|_, span| Spanned::from(span))
          .padded_by(super::ignored::padded()),
      )
      .then(value) // <-- use injected value parser
      .map_with(|((key, colon), value), sp| Self {
        key,
        colon,
        value,
        span: Spanned::from(sp),
      })
      .padded_by(super::ignored::padded())
  }
}

#[derive(Debug, Clone)]
pub enum AngleData<T, Src, Span> {
  Map(Vec<MapEntry<T, Src, Span>>),
  Set(Vec<T>),
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object T](https://spec.graphql.org/draft/#sec-Input-Object-T)
#[derive(Debug, Clone)]
pub struct Angle<T, Src, Span> {
  span: Spanned<Src, Span>,
  l_angle: LAngle<Spanned<Src, Span>>,
  r_angle: RAngle<Spanned<Src, Span>>,
  data: AngleData<T, Src, Span>,
}

impl<T, Src, Span> From<Angle<T, Src, Span>> for Either<Set<T, Src, Span>, Map<T, Src, Span>> {
  fn from(value: Angle<T, Src, Span>) -> Self {
    match value.data {
      AngleData::Map(fields) => Self::Right(Map {
        span: value.span,
        l_angle: value.l_angle,
        r_angle: value.r_angle,
        fields,
      }),
      AngleData::Set(values) => Self::Left(Set {
        span: value.span,
        l_angle: value.l_angle,
        r_angle: value.r_angle,
        values,
      }),
    }
  }
}

impl<T, Src, Span> Angle<T, Src, Span> {
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }

  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }

  pub const fn kind(&self) -> &AngleData<T, Src, Span> {
    &self.data
  }

  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, T, E> + Clone,
  {
    let ws = super::ignored::padded();
    let comma = just(I::Token::COMMA).padded_by(ws.clone());
    let colon = just(I::Token::COLON); // pad only where needed

    // Non-consuming guard: immediately after '<', do we have "value ':'"?
    let map_guard = value
      .clone()
      .padded_by(ws.clone())
      .then_ignore(colon.clone().padded_by(ws.clone()))
      .rewind();

    // Non-empty map: k:v pairs
    let non_empty_map = map_guard
      .ignore_then(
        MapEntry::<T, Src, Span>::parser_with(value.clone())
          .separated_by(comma.clone())
          .allow_trailing()
          .collect::<Vec<_>>(),
      )
      .map(AngleData::Map);

    // Non-empty set: plain values
    let non_empty_set = value
      .clone()
      .padded_by(ws.clone())
      .separated_by(comma.clone())
      .allow_trailing()
      .collect::<Vec<_>>()
      .map(AngleData::Set);

    // Empty map sentinel "<:>"
    let empty_map = colon
      .clone()
      .padded_by(ws.clone())
      .then_ignore(just(I::Token::GREATER_THAN).rewind())
      .map(|_| AngleData::Map(Vec::new()));

    // Empty set "<>"
    let empty_set = just(I::Token::GREATER_THAN)
      .rewind()
      .map(|_| AngleData::Set(Vec::new()));

    // Middle content chooser (no token consumption conflicts)
    let content = choice((empty_map, non_empty_map, non_empty_set, empty_set));

    // Wrap with '< ... >' and capture spans
    just(I::Token::LESS_THAN)
      .map_with(|_, sp| LAngle::new(Spanned::from(sp)))
      .then(content.padded_by(ws.clone()))
      .then(just(I::Token::GREATER_THAN).map_with(|_, sp| RAngle::new(Spanned::from(sp))))
      .map_with(|((l_angle, data), r_angle), sp| Self {
        span: Spanned::from(sp),
        l_angle,
        r_angle,
        data,
      })
  }
}

/// Represents an input object value parsed from input
///
/// Spec: [Input Object T](https://spec.graphql.org/draft/#sec-Input-Object-T)
#[derive(Debug, Clone)]
pub struct Map<T, Src, Span> {
  /// The original span of the object value
  span: Spanned<Src, Span>,
  /// The left `{` token.
  l_angle: LAngle<Spanned<Src, Span>>,
  /// The right `}` token.
  r_angle: RAngle<Spanned<Src, Span>>,
  /// The content between the brackets.
  fields: Vec<MapEntry<T, Src, Span>>,
}

impl<T, Src, Span> Map<T, Src, Span> {
  /// Returns the span of the object value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left angle of the object value.
  #[inline]
  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }

  /// Returns the right angle of the object value.
  #[inline]
  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }

  /// Returns the fields of the object value.
  #[inline]
  pub const fn fields(&self) -> &[MapEntry<T, Src, Span>] {
    self.fields.as_slice()
  }
}

/// Represents an set value parsed from input
///
/// Spec: [Set Value](https://spec.graphql.org/draft/#sec-Set-Value)
#[derive(Debug, Clone)]
pub struct Set<Value, Src, Span> {
  /// The original span of the set value
  span: Spanned<Src, Span>,
  /// The left `[` token.
  l_angle: LAngle<Spanned<Src, Span>>,
  /// The right `]` token.
  r_angle: RAngle<Spanned<Src, Span>>,
  /// The content between the brackets.
  ///
  /// Instead of parsing the set entirely, we keep the raw string representation.
  /// This allows for more efficient parsing and easier error reporting.
  /// The upper layers can perform the necessary parsing and validation.
  values: Vec<Value>,
}

impl<Value, Src, Span> Set<Value, Src, Span> {
  /// Returns the span of the set value.
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }

  /// Returns the left bracket of the set value.
  #[inline]
  pub const fn l_angle(&self) -> &LAngle<Spanned<Src, Span>> {
    &self.l_angle
  }

  /// Returns the right bracket of the set value.
  #[inline]
  pub const fn r_angle(&self) -> &RAngle<Spanned<Src, Span>> {
    &self.r_angle
  }

  /// Returns the values of the set.
  #[inline]
  pub const fn values(&self) -> &[Value] {
    self.values.as_slice()
  }
}
