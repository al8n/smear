use crate::parser::{
  language::punct::{LBrace, RBrace},
  Name, SmearChar, Spanned,
};
use chumsky::{
  extra::ParserExtra, input::StrInput, label::LabelError, prelude::*, text::TextExpected,
  util::MaybeRef,
};

#[derive(Debug, Clone)]
pub struct ObjectField<Value, Src, Span> {
  span: Spanned<Src, Span>,
  name: Name<Src, Span>,
  colon: Spanned<Src, Span>,
  value: Value,
}

impl<Value, Src, Span> ObjectField<Value, Src, Span> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn colon(&self) -> &Spanned<Src, Span> {
    &self.colon
  }
  #[inline]
  pub const fn name(&self) -> &Spanned<Src, Span> {
    self.name.span()
  }
  #[inline]
  pub const fn value(&self) -> &Value {
    &self.value
  }

  /// name ':' value  — only **trailing** ignored so repeats stop cleanly at `}`.
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
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
        just(I::Token::COLON)
          .map_with(|_, sp| Spanned::from(sp))
          .padded_by(ws.clone()), // padding *around* ':'
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
pub struct Object<Value, Src, Span, Container = std::vec::Vec<ObjectField<Value, Src, Span>>> {
  span: Spanned<Src, Span>,
  l_brace: LBrace<Spanned<Src, Span>>,
  r_brace: RBrace<Spanned<Src, Span>>,
  fields: Container,
  _value: core::marker::PhantomData<Value>,
}

impl<Value, Src, Span, Container> Object<Value, Src, Span, Container> {
  #[inline]
  pub const fn span(&self) -> &Spanned<Src, Span> {
    &self.span
  }
  #[inline]
  pub const fn l_brace(&self) -> &LBrace<Spanned<Src, Span>> {
    &self.l_brace
  }
  #[inline]
  pub const fn r_brace(&self) -> &RBrace<Spanned<Src, Span>> {
    &self.r_brace
  }
  #[inline]
  pub const fn fields(&self) -> &Container {
    &self.fields
  }
}

impl<Value, Src, Span, Container> Object<Value, Src, Span, Container>
where
  Container: chumsky::container::Container<ObjectField<Value, Src, Span>>,
{
  pub fn parser_with<'src, I, E, P>(value: P) -> impl Parser<'src, I, Self, E> + Clone
  where
    I: StrInput<'src, Slice = Src, Span = Span>,
    I::Token: SmearChar + 'src,
    Src: 'src,
    Span: 'src,
    E: ParserExtra<'src, I>,
    E::Error:
      LabelError<'src, I, TextExpected<'src, I>> + LabelError<'src, I, MaybeRef<'src, I::Token>>,
    P: Parser<'src, I, Value, E> + Clone,
  {
    let ws = super::ignored::ignored();
    let open = just(I::Token::CURLY_BRACE_OPEN).map_with(|_, sp| LBrace::new(Spanned::from(sp)));
    let close = just(I::Token::CURLY_BRACE_CLOSE).map_with(|_, sp| RBrace::new(Spanned::from(sp)));
    let field = ObjectField::<Value, Src, Span>::parser_with(value.clone()); // has trailing ws

    // '{' ws? ( '}' | field+ '}' )
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
