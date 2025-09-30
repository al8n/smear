use logosky::{
  Lexed, Logos, Parseable, Token,
  chumsky::{Parser, extra::ParserExtra, prelude::any},
  utils::{
    AsSpan, IntoComponents, IntoSpan, Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};
use smear_parser::lang;

use crate::{error::Error, lexer::ast::AstLexerErrors};

use super::*;

pub type TypeCondition<S> = lang::TypeCondition<Name<S>>;

impl<'a> Parseable<'a, StrAstTokenStream<'a>, StrAstToken<'a>, StrAstTokenErrors<'a, &'a str>>
  for FragmentName<&'a str>
{
  #[inline]
  fn parser<E>() -> impl Parser<'a, StrAstTokenStream<'a>, Self, E> + Clone
  where
    Self: Sized,
    E: ParserExtra<'a, StrAstTokenStream<'a>, Error = StrAstTokenErrors<'a, &'a str>> + 'a,
  {
    any().try_map(|res: Lexed<'_, StrAstToken<'_>>, span: Span| match res {
      Lexed::Token(tok) => {
        let (span, tok) = tok.into_components();
        match tok {
          StrAstToken::Identifier(name) => {
            if name.eq("on") {
              Err(Error::invalid_fragment_name(name, span).into())
            } else {
              Ok(FragmentName::new(span, name))
            }
          }
          tok => Err(Error::unexpected_token(tok, StrAstTokenKind::Identifier, span).into()),
        }
      }
      Lexed::Error(err) => Err(Error::from_lexer_errors(err, span).into()),
    })
  }
}

// impl<'a, S> Parseable<'a, AstTokenStream<'a, S>, AstToken<S>, AstTokenErrors<'a, S>>
//   for FragmentName<S>
// where
//   AstToken<S>: Token<'a>,
//   <AstToken<S> as Token<'a>>::Logos: Logos<'a, Error = AstLexerErrors<'a, S>>,
//   <<AstToken<S> as Token<'a>>::Logos as Logos<'a>>::Extras: Copy + 'a,
// {
//   #[inline]
//   fn parser<E>() -> impl Parser<'a, AstTokenStream<'a, S>, Self, E> + Clone
//   where
//     Self: Sized,
//     E: ParserExtra<'a, AstTokenStream<'a, S>, Error = AstTokenErrors<'a, S>> + 'a,
//   {
//     any().try_map(|res: Lexed<'_, AstToken<_>>, span: Span| match res {
//       Lexed::Token(tok) => {
//         let (span, tok) = tok.into_components();
//         match tok {
//           AstToken::Identifier(name) => {
//             if name.eq("on") {
//               Err(Error::invalid_fragment_name(name, span).into())
//             } else {
//               Ok(FragmentName::new(span, name))
//             }
//           }
//           tok => Err(Error::unexpected_token(tok, AstTokenKind::Identifier, span).into()),
//         }
//       }
//       Lexed::Error(err) => Err(AstTokenError::from_lexer_errors(err, span).into()),
//     })
//   }
// }

/// Represents a fragment name with the special restriction that it cannot be "on".
///
/// In GraphQL, fragment names are regular identifiers with one exception: they cannot
/// be the keyword "on" since this would create ambiguity with type conditions.
/// This type ensures this constraint is enforced at the parser level.
///
/// ## Examples
///
/// ```text
/// # Valid fragment names
/// UserFragment
/// productInfo
/// SearchResultFields
///
/// # Invalid fragment name (would be rejected)
/// on  # This is reserved for type conditions
/// ```
///
/// ## Grammar
///
/// ```text
/// FragmentName : Name but not `on`
/// ```
///
/// Spec: [Fragment Name](https://spec.graphql.org/draft/#FragmentName)
#[derive(Debug, Clone, Copy)]
pub struct FragmentName<S> {
  span: Span,
  value: S,
}

impl<S> PartialEq<S> for FragmentName<S>
where
  S: PartialEq,
{
  #[inline]
  fn eq(&self, other: &S) -> bool {
    self.slice_ref().eq(other)
  }
}

impl<S> From<FragmentName<S>> for Name<S> {
  #[inline]
  fn from(value: FragmentName<S>) -> Self {
    Self::new(value.span, value.value)
  }
}

impl<S> AsSpan<Span> for FragmentName<S> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<S> IntoSpan<Span> for FragmentName<S> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<S> IntoComponents for FragmentName<S> {
  type Components = Name<S>;

  #[inline]
  fn into_components(self) -> Self::Components {
    self.into()
  }
}

impl<S> FragmentName<S> {
  /// Creates a new fragment name.
  #[inline]
  pub const fn new(span: Span, value: S) -> Self {
    Self { span, value }
  }

  /// Returns a reference to the span covering the fragment name.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the underlying source value.
  #[inline]
  pub const fn slice(&self) -> S
  where
    S: Copy,
  {
    self.value
  }

  /// Returns a reference to the underlying source value.
  #[inline]
  pub const fn slice_ref(&self) -> &S {
    &self.value
  }
}

impl<S> core::fmt::Display for FragmentName<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplayHuman::fmt(self.slice_ref(), f)
  }
}

impl<S> core::ops::Deref for FragmentName<S> {
  type Target = S;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.slice_ref()
  }
}

impl<S> DisplaySDL for FragmentName<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    self.value.fmt(f)
  }
}

impl<S> DisplaySyntaxTree for FragmentName<S>
where
  S: DisplayHuman,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let mut padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- FRAGMENT_NAME@{}..{}",
      self.span.start(),
      self.span.end()
    )?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(f, "- NAME@{}..{}", self.span.start(), self.span.end())?;
    padding += indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    write!(
      f,
      "- IDENT@{}..{} \"{}\"",
      self.span.start(),
      self.span.end(),
      self.value.display(),
    )
  }
}
