use core::fmt::Display;

use chumsky::{Parser, extra::ParserExtra};
use logosky::{
  Parseable, Source, Token, Tokenizer,
  utils::{
    Span, human_display::DisplayHuman, sdl_display::DisplaySDL,
    syntax_tree_display::DisplaySyntaxTree,
  },
};

use crate::{
  error::{ParseVariableValueError, VariableValueHint},
  lang::punctuator::Dollar,
};

/// A GraphQL variable reference.
///
/// Represents a variable reference as defined by the GraphQL specification.
/// Variables are placeholders in GraphQL operations that get their values
/// from variables passed during execution. They enable parameterized queries
/// and mutations without requiring string interpolation or dynamic query
/// construction.
///
/// ## Specification Rules
///
/// GraphQL variable references follow strict formatting rules:
/// - **Dollar prefix**: Must start with a `$` character
/// - **Variable name**: Followed by a valid GraphQL name identifier
/// - **Case-sensitive**: Variable names are case-sensitive identifiers
/// - **Scope-aware**: Variables must be declared in the operation signature
///
/// ## Grammar
///
/// ```text
/// Variable ::= '$' Name
/// ```
///
/// ## Component Structure
///
/// Each variable reference contains:
/// - **Overall span**: Covers the entire variable including `$` and name
/// - **Dollar token**: The `$` prefix character with its position
/// - **Variable name**: The identifier following the `$`
///
/// ## Variable Declaration and Usage
///
/// Variables must be declared in operation signatures before use:
/// ```text
/// query GetUser($userId: ID!, $includeProfile: Boolean = false) {
///   user(id: $userId) {
///     name
///     profile @include(if: $includeProfile) {
///       bio
///     }
///   }
/// }
/// ```
///
/// Spec: [Variable Value](https://spec.graphql.org/draft/#sec-Variable-Value)
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Variable<Name> {
  span: Span,
  name: Name,
}

impl<Name> AsRef<Span> for Variable<Name> {
  #[inline]
  fn as_ref(&self) -> &Span {
    self.span()
  }
}

impl<Name> core::ops::Deref for Variable<Name> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name> Display for Variable<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    DisplaySDL::fmt(self, f)
  }
}

impl<Name> Variable<Name> {
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

impl<Name> DisplaySDL for Variable<Name>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "${}", self.name.display())
  }
}

impl<Name> DisplaySyntaxTree for Variable<Name>
where
  Name: DisplaySyntaxTree,
{
  #[inline]
  fn fmt(
    &self,
    level: usize,
    indent: usize,
    f: &mut core::fmt::Formatter<'_>,
  ) -> core::fmt::Result {
    let padding = level * indent;
    write!(f, "{:indent$}", "", indent = padding)?;
    writeln!(
      f,
      "- VARIABLE@{}..{}",
      self.span().start(),
      self.span().end()
    )?;
    <Name as DisplaySyntaxTree>::fmt(self.name(), level + 1, indent, f)
  }
}

impl<'a, Name, I, T, Error> Parseable<'a, I, T, Error> for Variable<Name>
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
    I: Tokenizer<'a, T, Slice = <T::Source as Source>::Slice<'a>>,
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
          (Some(_), Some(name)) => Ok(Variable { span, name }),
        }
      })
  }
}
