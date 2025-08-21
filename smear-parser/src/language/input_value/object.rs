use chumsky::{extra::ParserExtra, label::LabelError, prelude::*};

use super::super::{
  super::{char::Char, name::Name, source::Source, spanned::Spanned, convert::*},
  punct::{Colon, LBrace, RBrace},
};

/// A single field within a GraphQL input object literal.
///
/// Represents a name-value pair within an object literal, following the
/// GraphQL specification for input object fields. Each field consists of
/// a field name, a colon separator, and a value, with optional whitespace
/// and comments allowed around each component.
///
/// ## Format
///
/// ```text
/// ObjectField ::= Name ':' Value
/// ```
///
/// ## Examples
///
/// ```text
/// name: "John"              // String field
/// age: 25                   // Integer field  
/// active: true              // Boolean field
/// tags: ["user", "admin"]   // List field
/// profile: { bio: "..." }   // Nested object field
/// settings: null            // Null field
/// ```
///
/// ## Component Structure
///
/// Each field contains:
/// - **Overall span**: Covers from field name through the value
/// - **Field name**: A GraphQL name identifier
/// - **Colon separator**: The `:` token with its position
/// - **Field value**: The value assigned to this field
#[derive(Debug, Clone)]
pub struct ObjectValueField<InputValue, Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Colon<Src, Span>,
  value: InputValue,
}

impl<InputValue, Src, Span> AsSpanned<Src, Span> for ObjectValueField<InputValue, Src, Span> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    &self.span
  }
}

impl<InputValue, Src, Span> IntoSpanned<Src, Span> for ObjectValueField<InputValue, Src, Span> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<InputValue, Src, Span> IntoComponents for ObjectValueField<InputValue, Src, Span> {
  type Components = (Spanned<Src, Span>, Name<Src, Span>, Colon<Src, Span>, InputValue);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name, self.colon, self.value)
  }
}

impl<InputValue, Src, Span> ObjectValueField<InputValue, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn colon(&self) -> &Colon<Src, Span> {
    &self.colon
  }
  #[inline]
  pub const fn name(&self) -> &Name<Src, Span> {
    &self.name
  }
  #[inline]
  pub const fn value(&self) -> &InputValue {
    &self.value
  }

  /// name ':' value  — only **trailing** ignored so repeats stop cleanly at `}`.
  pub fn parser_with<'src, I, E, P, const CONST: bool>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    P: Parser<'src, I, InputValue, E> + Clone,
    InputValue: crate::language::input_value::InputValue<CONST>,
  {
    let ws = super::ignored::ignored();

    Name::parser()
      .then(
        Colon::parser().padded_by(ws.clone()),
      )
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Spanned::from(sp),
        name,
        colon,
        value,
      })
      .labelled(if CONST {
        "const object field"
      } else {
        "object field"
      })
  }
}

/// Input Object Value: `{` (fields)? `}`
#[derive(Debug, Clone)]
pub struct ObjectValue<
  InputValue,
  Src,
  Span,
  Container = std::vec::Vec<ObjectValueField<InputValue, Src, Span>>,
> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  fields: Container,
  _value: core::marker::PhantomData<InputValue>,
}

impl<InputValue, Src, Span, Container> AsSpanned<Src, Span> for ObjectValue<InputValue, Src, Span, Container> {
  #[inline]
  fn as_spanned(&self) -> &Spanned<Src, Span> {
    &self.span
  }
}

impl<InputValue, Src, Span, Container> IntoSpanned<Src, Span> for ObjectValue<InputValue, Src, Span, Container> {
  #[inline]
  fn into_spanned(self) -> Spanned<Src, Span> {
    self.span
  }
}

impl<InputValue, Src, Span, Container> IntoComponents for ObjectValue<InputValue, Src, Span, Container> {
  type Components = (Spanned<Src, Span>, LBrace<Src, Span>, Container, RBrace<Src, Span>);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.l_brace, self.fields, self.r_brace)
  }
}

impl<InputValue, Src, Span, Container> ObjectValue<InputValue, Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Src, Span> {
    &self.l_brace
  }
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Src, Span> {
    &self.r_brace
  }
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }
}

impl<InputValue, Src, Span, Container> ObjectValue<InputValue, Src, Span, Container>
where
  Container: chumsky::container::Container<ObjectValueField<InputValue, Src, Span>>,
{
  pub fn parser_with<'src, I, E, P, const CONST: bool>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: Source<'src, Slice = Src, Span = Span>,
    I::Token: Char + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error: LabelError<'src, I, &'static str>,
    P: Parser<'src, I, InputValue, E> + Clone,
    InputValue: crate::language::input_value::InputValue<CONST>
  {
    let ws = super::ignored::ignored();
    let open = LBrace::parser();
    let close = RBrace::parser();
    let field = ObjectValueField::<InputValue, Src, Span>::parser_with(value.clone());

    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty fast path: immediately '}'
        close.clone().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more fields; commas are in `ws`
        field.padded_by(ws).repeated().at_least(1).collect().then(close.clone()),
      )))
      .map_with(|(l_brace, (fields, r_brace)), sp| Self {
        span: Spanned::from(sp),
        l_brace,
        r_brace,
        fields,
        _value: core::marker::PhantomData,
      })
      .labelled(if CONST {
        "const object value"
      } else {
        "object value"
      })
  }
}
