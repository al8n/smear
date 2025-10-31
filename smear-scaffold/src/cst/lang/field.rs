use logosky::utils::Span;

use crate::cst::Padding;

/// CST representation of a field alias in GraphQL.
///
/// Unlike the AST version, this preserves:
/// - The name token with its padding
/// - The colon token with its padding
///
/// ## Examples
/// ```text
/// user: profile        # name: "user", colon has right padding " "
/// primaryEmail :email  # name: "primaryEmail", colon has left padding " "
/// ```
#[derive(Debug, Clone)]
pub struct Alias<Name, S, TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>> {
  span: Span,
  /// The alias name with its padding
  name: Name,
  /// Padding around the colon separator
  colon_padding: Padding<S, TriviaContainer>,
}

impl<Name, S, TriviaContainer> Alias<Name, S, TriviaContainer>
where
  TriviaContainer: Default,
{
  /// Creates a new CST Alias.
  pub fn new(span: Span, name: Name) -> Self {
    Self {
      span,
      name,
      colon_padding: Padding::new(),
    }
  }

  /// Creates a new CST Alias with the given colon padding.
  pub const fn with_colon_padding(
    span: Span,
    name: Name,
    colon_padding: Padding<S, TriviaContainer>,
  ) -> Self {
    Self {
      span,
      name,
      colon_padding,
    }
  }
}

impl<Name, S, TriviaContainer> Alias<Name, S, TriviaContainer> {
  /// Returns the span covering the entire alias.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the alias name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the colon padding.
  #[inline]
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> {
    &self.colon_padding
  }

  /// Returns a mutable reference to the name.
  #[inline]
  pub fn name_mut(&mut self) -> &mut Name {
    &mut self.name
  }

  /// Returns a mutable reference to the colon padding.
  #[inline]
  pub fn colon_padding_mut(&mut self) -> &mut Padding<S, TriviaContainer> {
    &mut self.colon_padding
  }
}

impl<Name, S, TriviaContainer> core::ops::Deref for Alias<Name, S, TriviaContainer> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

/// CST representation of a GraphQL field.
///
/// Unlike the AST version which only stores semantic information, the CST version
/// preserves all tokens and trivia:
/// - Alias name and colon (if present)
/// - Field name
/// - Opening and closing parentheses for arguments (if present)
/// - All directives
/// - Opening and closing braces for selection set (if present)
/// - All whitespace and comments between tokens
///
/// ## Examples
/// ```text
/// # Simple field
/// name
///
/// # Field with alias
/// userName: name
///
/// # Field with arguments
/// user(id: "123")
///
/// # Field with selection set
/// user {
///   id
///   name
/// }
///
/// # Complex field
/// primaryUser: user(id: "123") @include(if: true) {
///   id
///   profile {
///     name
///   }
/// }
/// ```
#[derive(Debug, Clone)]
pub struct Field<
  Alias,
  Name,
  Arguments,
  Directives,
  SelectionSet,
  S,
  TriviaContainer = std::vec::Vec<crate::cst::Trivia<S>>,
> {
  span: Span,
  /// Optional alias with its trivia
  alias: Option<Alias>,
  /// Field name with its padding
  name: Name,
  /// Optional arguments with parentheses and padding
  arguments: Option<Arguments>,
  /// Optional directives with padding
  directives: Option<Directives>,
  /// Optional selection set with braces and padding
  selection_set: Option<SelectionSet>,
  _marker: core::marker::PhantomData<(S, TriviaContainer)>,
}

impl<Alias, Name, Arguments, Directives, SelectionSet, S, TriviaContainer>
  Field<Alias, Name, Arguments, Directives, SelectionSet, S, TriviaContainer>
{
  /// Creates a new CST Field with the given components.
  pub const fn new(
    span: Span,
    alias: Option<Alias>,
    name: Name,
    arguments: Option<Arguments>,
    directives: Option<Directives>,
    selection_set: Option<SelectionSet>,
  ) -> Self {
    Self {
      span,
      alias,
      name,
      arguments,
      directives,
      selection_set,
      _marker: core::marker::PhantomData,
    }
  }

  /// Returns the span covering the entire field.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the field's alias, if present.
  #[inline]
  pub const fn alias(&self) -> Option<&Alias> {
    self.alias.as_ref()
  }

  /// Returns a reference to the field's name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }

  /// Returns a reference to the field's arguments, if present.
  #[inline]
  pub const fn arguments(&self) -> Option<&Arguments> {
    self.arguments.as_ref()
  }

  /// Returns a reference to the field's directives, if present.
  #[inline]
  pub const fn directives(&self) -> Option<&Directives> {
    self.directives.as_ref()
  }

  /// Returns a reference to the field's selection set, if present.
  #[inline]
  pub const fn selection_set(&self) -> Option<&SelectionSet> {
    self.selection_set.as_ref()
  }

  /// Returns mutable references to all field components.
  pub fn as_mut_parts(
    &mut self,
  ) -> (
    &mut Option<Alias>,
    &mut Name,
    &mut Option<Arguments>,
    &mut Option<Directives>,
    &mut Option<SelectionSet>,
  ) {
    (
      &mut self.alias,
      &mut self.name,
      &mut self.arguments,
      &mut self.directives,
      &mut self.selection_set,
    )
  }

  /// Deconstructs the Field into its components.
  pub fn into_parts(
    self,
  ) -> (
    Span,
    Option<Alias>,
    Name,
    Option<Arguments>,
    Option<Directives>,
    Option<SelectionSet>,
  ) {
    (
      self.span,
      self.alias,
      self.name,
      self.arguments,
      self.directives,
      self.selection_set,
    )
  }
}
