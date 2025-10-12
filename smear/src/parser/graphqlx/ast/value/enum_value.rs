use derive_more::{From, Into};
use logosky::{
  Logos, Parseable, Source, Token, Tokenizer,
  chumsky::{Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, cmp::Equivalent},
};

use crate::{lexer::graphqlx::ast::AstLexerErrors, parser::graphqlx::error::InvalidEnumValue};

use super::super::*;

// pub use crate::parser::value::EnumValue;

#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct EnumValue<S>(Path<S>);

impl<S> AsSpan<Span> for EnumValue<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.0.as_span()
  }
}

impl<S> IntoSpan<Span> for EnumValue<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.0.into_span()
  }
}

impl<S> IntoComponents for EnumValue<S> {
  type Components = (Span, bool, DefaultVec<Ident<S>>);

  #[inline]
  fn into_components(self) -> Self::Components {
    self.0.into_components()
  }
}

impl<S> EnumValue<S> {
  /// Creates a new enum value.
  #[inline]
  pub(super) const fn new(path: Path<S>) -> Self {
    Self(path)
  }

  /// Returns the span of the enum value.
  #[inline]
  pub fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the value of the enum.
  #[inline]
  pub fn value(&self) -> &Path<S> {
    &self.0
  }
}

impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
  for EnumValue<S>
where
  AstToken<S>: Token<'a>,
  <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
  <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Path<S>: Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>,
  str: Equivalent<Path<S>>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    AstTokenStream<'a, S>: Tokenizer<
        'a,
        AstToken<S>,
        Slice = <<<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<'a>,
      >,
    AstToken<S>: Token<'a>,
    AstTokenErrors<'a, S>: 'a,
    E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
  {
    Path::parser().try_map(|path, span: Span| {
      if "true".equivalent(&path) {
        return Err(Error::invalid_enum_value(InvalidEnumValue::True, span).into());
      }

      if "false".equivalent(&path) {
        return Err(Error::invalid_enum_value(InvalidEnumValue::False, span).into());
      }

      if "null".equivalent(&path) {
        return Err(Error::invalid_enum_value(InvalidEnumValue::Null, span).into());
      }

      Ok(EnumValue::new(path))
    })
  }
}
