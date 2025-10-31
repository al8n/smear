use logosky::utils::Span;
use std::vec::Vec;

/// Represents a single piece of trivia (whitespace or comment).
///
/// Trivia includes all syntactically insignificant tokens that are skipped
/// by the AST parser but must be preserved for lossless source reconstruction.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum Trivia<S> {
  /// Single space character ` `
  Space,
  /// Tab character `\t`
  Tab,
  /// Newline character `\n`
  Newline,
  /// Carriage return character `\r`
  CarriageReturn,
  /// Carriage return + newline `\r\n`
  CarriageReturnAndNewline,
  /// Comment including the leading `#` and content
  Comment(S),
  /// Comma `,` separator
  Comma,
  /// Byte Order Mark `\u{FEFF}`
  Bom(S),
}

impl<S> Trivia<S> {
  /// Returns true if this trivia represents whitespace.
  pub const fn is_whitespace(&self) -> bool {
    matches!(
      self,
      Self::Space
        | Self::Tab
        | Self::Newline
        | Self::CarriageReturn
        | Self::CarriageReturnAndNewline
    )
  }

  /// Returns true if this trivia is a comment.
  pub const fn is_comment(&self) -> bool {
    matches!(self, Self::Comment(_))
  }

  /// Returns true if this trivia is a comma.
  pub const fn is_comma(&self) -> bool {
    matches!(self, Self::Comma)
  }
}

/// Padding represents trivia (whitespace, comments, commas) around tokens.
///
/// This structure stores both leading and trailing trivia, allowing CST nodes
/// to preserve all formatting information between significant tokens.
///
/// # Examples
///
/// ```text
/// // For the GraphQL: "  name  # field comment\n  "
/// // The 'name' token would have:
/// // - left padding: ["  "]
/// // - right padding: ["  ", Comment("# field comment"), "\n", "  "]
/// ```
#[derive(Debug, Clone, Default, PartialEq, Eq)]
pub struct Padding<S, Container = Vec<Trivia<S>>> {
  /// Trivia appearing before the token
  left: Container,
  /// Trivia appearing after the token
  right: Container,
  _marker: core::marker::PhantomData<S>,
}

impl<S, Container> Padding<S, Container>
where
  Container: Default,
{
  /// Creates a new empty Padding with no trivia.
  pub fn new() -> Self {
    Self {
      left: Container::default(),
      right: Container::default(),
      _marker: core::marker::PhantomData,
    }
  }

  /// Creates a new Padding with the given left and right trivia.
  pub const fn with_trivia(left: Container, right: Container) -> Self {
    Self {
      left,
      right,
      _marker: core::marker::PhantomData,
    }
  }

  /// Creates a new Padding with only left trivia.
  pub fn with_left(left: Container) -> Self {
    Self {
      left,
      right: Container::default(),
      _marker: core::marker::PhantomData,
    }
  }

  /// Creates a new Padding with only right trivia.
  pub fn with_right(right: Container) -> Self {
    Self {
      left: Container::default(),
      right,
      _marker: core::marker::PhantomData,
    }
  }
}

impl<S, Container> Padding<S, Container> {
  /// Returns a reference to the left trivia.
  pub const fn left(&self) -> &Container {
    &self.left
  }

  /// Returns a reference to the right trivia.
  pub const fn right(&self) -> &Container {
    &self.right
  }

  /// Returns a mutable reference to the left trivia.
  pub fn left_mut(&mut self) -> &mut Container {
    &mut self.left
  }

  /// Returns a mutable reference to the right trivia.
  pub fn right_mut(&mut self) -> &mut Container {
    &mut self.right
  }

  /// Deconstructs the Padding into its left and right components.
  pub fn into_parts(self) -> (Container, Container) {
    (self.left, self.right)
  }
}

/// A token with associated padding (trivia before and after).
///
/// This wraps any token type with padding information, allowing CST nodes
/// to store formatting alongside each token.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct Padded<T, S, Container = Vec<Trivia<S>>> {
  span: Span,
  padding: Padding<S, Container>,
  token: T,
}

impl<T, S, Container> Padded<T, S, Container>
where
  Container: Default,
{
  /// Creates a new Padded token without any trivia.
  pub fn new(span: Span, token: T) -> Self {
    Self {
      span,
      padding: Padding::new(),
      token,
    }
  }

  /// Creates a new Padded token with the given padding.
  pub const fn with_padding(span: Span, padding: Padding<S, Container>, token: T) -> Self {
    Self {
      span,
      padding,
      token,
    }
  }
}

impl<T, S, Container> Padded<T, S, Container> {
  /// Returns the span of this padded token.
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns a reference to the padding.
  pub const fn padding(&self) -> &Padding<S, Container> {
    &self.padding
  }

  /// Returns a reference to the underlying token.
  pub const fn token(&self) -> &T {
    &self.token
  }

  /// Returns a mutable reference to the padding.
  pub fn padding_mut(&mut self) -> &mut Padding<S, Container> {
    &mut self.padding
  }

  /// Returns a mutable reference to the underlying token.
  pub fn token_mut(&mut self) -> &mut T {
    &mut self.token
  }

  /// Deconstructs the Padded into its components.
  pub fn into_parts(self) -> (Span, Padding<S, Container>, T) {
    (self.span, self.padding, self.token)
  }
}

impl<T, S, Container> core::ops::Deref for Padded<T, S, Container> {
  type Target = T;

  fn deref(&self) -> &Self::Target {
    &self.token
  }
}

impl<T, S, Container> core::ops::DerefMut for Padded<T, S, Container> {
  fn deref_mut(&mut self) -> &mut Self::Target {
    &mut self.token
  }
}
