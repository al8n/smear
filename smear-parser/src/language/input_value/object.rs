use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

use super::super::{
  super::{char::Char, name::Name, spanned::Spanned},
  punct::{Colon, LBrace, RBrace},
};

#[derive(Debug, Clone)]
pub struct ObjectValueField<Value, Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Colon<Src, Span>,
  value: Value,
}

impl<Value, Src, Span> ObjectValueField<Value, Src, Span> {
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
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// name ':' value  — only **trailing** ignored so repeats stop cleanly at `}`.
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
    let ws = super::ignored::ignored();

    Name::<Src, Span>::parser()
      .then(
        Colon::parser().padded_by(ws.clone()), // padding *around* ':'
      )
      .then(value)
      .map_with(|((name, colon), value), sp| Self {
        span: Spanned::from(sp),
        name,
        colon,
        value,
      })
      .then_ignore(ws) // <-- trailing only (incl. commas)
  }
}

/// Input Object Value: `{` (fields)? `}`
#[derive(Debug, Clone)]
pub struct ObjectValue<
  Value,
  Src,
  Span,
  Container = std::vec::Vec<ObjectValueField<Value, Src, Span>>,
> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Src, Span>,
  r_brace: RBrace<Src, Span>,
  fields: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Src, Span, Container> ObjectValue<Value, Src, Span, Container> {
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

impl<Value, Src, Span, Container> ObjectValue<Value, Src, Span, Container>
where
  Container: chumsky::container::Container<ObjectValueField<Value, Src, Span>>,
{
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
    let ws = super::ignored::ignored();
    let open = LBrace::parser();
    let close = RBrace::parser();
    let field = ObjectValueField::<Value, Src, Span>::parser_with(value.clone());

    open
      .then_ignore(ws.clone())
      .then(choice((
        // Empty fast path: immediately '}'
        close.clone().map(|r| (Container::default(), r)),
        // Non-empty: one-or-more fields; commas are in `ws`
        field.repeated().at_least(1).collect().then(close.clone()),
      )))
      .map_with(|(l_brace, (fields, r_brace)), sp| Self {
        span: Spanned::from(sp),
        l_brace,
        r_brace,
        fields,
        _value: core::marker::PhantomData,
      })
  }
}
