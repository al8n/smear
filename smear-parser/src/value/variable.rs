use core::fmt::Display;

use tokora::{
  SimpleSpan,
  span::{AsSpan, IntoSpan},
  utils::{
    IntoComponents,
    human_display::DisplayHuman,
    sdl_display::{DisplayCompact, DisplayPretty},
  },
};

/// A variable value.
#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct VariableValue<Name, Span = SimpleSpan> {
  span: Span,
  name: Name,
}

impl<Name, Span> AsSpan<Span> for VariableValue<Name, Span> {
  #[inline]
  fn as_span(&self) -> &Span {
    self.span()
  }
}

impl<Name, Span> IntoSpan<Span> for VariableValue<Name, Span> {
  #[inline]
  fn into_span(self) -> Span {
    self.span
  }
}

impl<Name, Span> IntoComponents for VariableValue<Name, Span> {
  type Components = (Span, Name);

  #[inline]
  fn into_components(self) -> Self::Components {
    (self.span, self.name)
  }
}

impl<Name, Span> core::ops::Deref for VariableValue<Name, Span> {
  type Target = Name;

  #[inline]
  fn deref(&self) -> &Self::Target {
    self.name()
  }
}

impl<Name, Span> Display for VariableValue<Name, Span>
where
  Name: DisplayHuman,
{
  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
    write!(f, "${}", self.name.display())
  }
}

impl<Name, Span> VariableValue<Name, Span> {
  /// Creates a new variable from the given span and name.
  ///
  /// Public because every other node of this AST is constructible — [`Name::new`], `Field::new`,
  /// `Argument::new`, `Document::new` — and a document assembled rather than parsed is a
  /// supported input: `graphql_proto::Executor::new` accepts any `&ExecutableDocument<S>`
  /// whatever built it. This one was `pub(crate)` with no recorded reason, which left `$name` the
  /// single production a rewriter, a persisted-query store or an FFI bridge could not spell, and
  /// made a whole class of executor input unreachable *and* untestable rather than merely
  /// unusual.
  ///
  /// [`FragmentName::new`](crate::name::FragmentName) is deliberately not public and stays so:
  /// its exclusion of `on` is a grammar rule the syntactic productions establish, and a
  /// constructor would let a caller past it. There is no analogous rule here — draft §2.1.9's
  /// name production is [`Name`]'s, and this node adds only the `$`.
  #[inline]
  pub const fn new(span: Span, name: Name) -> Self {
    Self { span, name }
  }

  /// Returns the span covering the variable.
  #[inline]
  pub const fn span(&self) -> &Span {
    &self.span
  }

  /// Returns the variable name.
  #[inline]
  pub const fn name(&self) -> &Name {
    &self.name
  }
}

impl<Name, Span> DisplayCompact for VariableValue<Name, Span>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}

impl<Name, Span> DisplayPretty for VariableValue<Name, Span>
where
  Name: DisplayHuman,
{
  type Options = ();

  #[inline]
  fn fmt(&self, f: &mut core::fmt::Formatter<'_>, _: &Self::Options) -> core::fmt::Result {
    core::fmt::Display::fmt(self, f)
  }
}
