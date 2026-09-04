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
  /// # A deliberate exception to this crate's rule, and the rule is real
  ///
  /// An earlier revision of this comment said the node was `pub(crate)` "with no recorded reason"
  /// and that every other node of this AST is constructible. Both halves are wrong, and the
  /// compiler says so: [`IntValue`](crate::value::IntValue),
  /// [`FloatValue`](crate::value::FloatValue), [`StringValue`](crate::value::StringValue),
  /// [`BooleanValue`](crate::value::BooleanValue), [`NullValue`](crate::value::NullValue) and
  /// [`EnumValue`](crate::value::EnumValue) all keep `new` crate-private, so an
  /// `InputValue` built from outside this crate cannot be a scalar literal at all — `f(x: 1)` is
  /// unspellable. The rule the crate actually applies is that a node **carrying a lexeme** is
  /// constructed by the production that lexed it, and a **structural** node — `Field`, `Argument`,
  /// `List`, `Object`, `SelectionSet`, `Document` — is public. This node is `$` and a lexeme, so
  /// the rule puts it with the leaves.
  ///
  /// It is public anyway, for one reason that survives: `graphql_proto::Executor::new` accepts any
  /// `&ExecutableDocument<S>` for any `S: AsRef<[u8]>`, and what execution does with a spelling
  /// that is not a draft §2.1.9 `Name` is behaviour that has to be *writable* to be pinned. It is
  /// pinned, in `graphql-proto/tests/unreadable_name.rs`, and every fixture there needs this
  /// constructor. The narrower door — keep this crate-private and add a checked builder — was
  /// considered and does not pay for itself: `Executor::new`'s bound is a caller-implemented
  /// trait with no purity requirement, so a check here binds nothing the executor reads later;
  /// this node adds no rule of its own to check; and the sibling names inside the very same
  /// literal, an [`Object`](crate::value::Object) field's or an alias's, are built from the public
  /// and unchecked [`Name::new`] regardless.
  ///
  /// So what is missing is an admission story for the AST as a whole — a `Name` that is checked
  /// once at its own door, and value leaves an assembler can spell — and not a guard on this one
  /// node. Until there is one, this constructor admits exactly what [`Name::new`] admits, and the
  /// refusal that matters is `graphql_proto::variable_key`'s, on the side that owns the key space.
  ///
  /// [`FragmentName::new`](crate::name::FragmentName) is deliberately not public and stays so:
  /// its exclusion of `on` is a grammar rule the syntactic productions establish, and a
  /// constructor would let a caller past it. There is no analogous rule here — draft §2.1.9's
  /// name production is [`Name`]'s, and this node adds only the `$`.
  #[inline]
  pub const fn new(span: Span, name: Name) -> Self
  where
    Span: crate::value::Leaf,
  {
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
