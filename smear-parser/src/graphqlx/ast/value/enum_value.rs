use derive_more::{From, Into};
use logosky::{
  Logos, Source, Token, Tokenizer,
  chumsky::{Parseable, Parser, extra::ParserExtra},
  utils::{AsSpan, IntoComponents, IntoSpan, Span, cmp::Equivalent},
};
use smear_lexer::graphqlx::syntactic::SyntacticLexerErrors;

use crate::{graphqlx::error::InvalidEnumValue, value::EnumValue as EnumValueInner};

use super::super::*;

/// An enum value in GraphQLx.
#[derive(Debug, Clone, PartialEq, Eq, Hash, From, Into)]
pub struct EnumValue<S>(EnumValueInner<Path<S>>);

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
    let (_, path) = self.0.into_components();
    path.into_components()
  }
}

impl<S> EnumValue<S> {
  /// Creates a new enum value.
  #[inline]
  pub(super) const fn new(path: Path<S>) -> Self {
    Self(EnumValueInner::new(*path.span(), path))
  }

  /// Returns the span of the enum value.
  #[inline]
  pub fn span(&self) -> &Span {
    self.0.span()
  }

  /// Returns the value of the enum.
  #[inline]
  pub const fn value(&self) -> &Path<S> {
    self.0.source_ref()
  }
}

impl<'a, S>
  Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>
  for EnumValue<S>
where
  SyntacticToken<S>: Token<'a>,
  <SyntacticToken<S> as Token<'a>>::Logos: Logos<'a, Error = SyntacticLexerErrors<'a, S>>,
  <<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
  Path<S>:
    Parseable<'a, SyntacticTokenStream<'a, S>, SyntacticToken<S>, SyntacticTokenErrors<'a, S>>,
  str: Equivalent<Path<S>>,
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, SyntacticTokenStream<'a, S>, Self, E> + Clone
  where
    Self: Sized + 'a,
    SyntacticTokenStream<'a, S>: Tokenizer<
        'a,
        SyntacticToken<S>,
        Slice = <<<SyntacticToken<S> as Token<'a>>::Logos as Logos<'a>>::Source as Source>::Slice<
          'a,
        >,
      >,
    SyntacticToken<S>: Token<'a>,
    SyntacticTokenErrors<'a, S>: 'a,
    E: ParserExtra<'a, SyntacticTokenStream<'a, S>, Error = SyntacticTokenErrors<'a, S>> + 'a,
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
