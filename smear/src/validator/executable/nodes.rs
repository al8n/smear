//! The two small abstractions the rules need over the syntactic AST, and the accessors that keep
//! the traversal readable.
//!
//! # Why any abstraction at all
//!
//! GraphQL writes the same argument and directive grammar twice: once where variables may appear
//! (fields, spreads, inline fragments, operations, fragment definitions) and once where they may
//! not (a variable definition's default value and its directives). The AST spells those as two
//! type families — `InputValue`/`ConstInputValue`, `Directive`/`ConstDirective` — which differ in
//! exactly one variant. [`ValueLike`] and [`DirectiveLike`] name what the rules use, so draft
//! 5.4.x, 5.6.x and 5.7.x are written once and instantiated twice instead of being copied and
//! left to drift.
//!
//! Neither trait is public. They abstract over two known types, not over an open set, and the
//! `Option`-returning shape they have would be wrong for anything else — it exists because a
//! constant value simply has no variable arm to return.

use tokora::{SimpleSpan, span::AsSpan};

use crate::parser::graphql::ast::{
  Argument, BooleanValue, ConstArgument, ConstDirective, ConstInputValue, ConstObjectField,
  Directive, EnumValue, ExecutableDefinition, ExecutableDocument, FloatValue, FragmentDefinition,
  InputValue, IntValue, Name, ObjectField, OperationDefinition, Selection, SelectionSet,
  StringValue, VariableValue,
};

/// A GraphQL input value, whether or not it may contain variables.
pub(crate) trait ValueLike<S>: Sized {
  /// The object-field row type an object literal of this family holds.
  type Field: ObjectFieldLike<S, Value = Self>;

  /// Returns the span covering the value.
  fn value_span(&self) -> SimpleSpan;
  /// Returns the variable, when the value is one.
  ///
  /// Always `None` for a constant value, which is what makes draft 5.8's rules vanish from the
  /// constant instantiation rather than needing to be switched off.
  fn as_variable(&self) -> Option<&VariableValue<S>>;
  /// Returns whether the value is the `null` literal.
  fn is_null(&self) -> bool;
  /// Returns the boolean literal, when the value is one.
  fn as_boolean(&self) -> Option<&BooleanValue<S>>;
  /// Returns the integer literal, when the value is one.
  fn as_int(&self) -> Option<&IntValue<S>>;
  /// Returns the float literal, when the value is one.
  fn as_float(&self) -> Option<&FloatValue<S>>;
  /// Returns the string literal, when the value is one.
  fn as_string(&self) -> Option<&StringValue<S>>;
  /// Returns the enum literal, when the value is one.
  fn as_enum(&self) -> Option<&EnumValue<S>>;
  /// Returns the list's entries, when the value is a list literal.
  fn as_list(&self) -> Option<&[Self]>;
  /// Returns the object's fields, when the value is an object literal.
  fn as_object(&self) -> Option<&[Self::Field]>;
}

/// One `Name : Value` row of an object literal.
pub(crate) trait ObjectFieldLike<S> {
  /// The value type the row carries.
  type Value;

  /// Returns the span covering the row.
  fn field_span(&self) -> SimpleSpan;
  /// Returns the field's name.
  fn field_name(&self) -> &Name<S>;
  /// Returns the field's value.
  fn field_value(&self) -> &Self::Value;
}

/// One `Name : Value` argument of a field or directive.
pub(crate) trait ArgumentLike<S> {
  /// The value type the argument carries.
  type Value: ValueLike<S>;

  /// Returns the span covering the argument.
  fn argument_span(&self) -> SimpleSpan;
  /// Returns the argument's name.
  fn argument_name(&self) -> &Name<S>;
  /// Returns the argument's value.
  fn argument_value(&self) -> &Self::Value;
}

impl<S> ObjectFieldLike<S> for ObjectField<S> {
  type Value = InputValue<S>;

  #[inline]
  fn field_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn field_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn field_value(&self) -> &Self::Value {
    self.value()
  }
}

impl<S> ObjectFieldLike<S> for ConstObjectField<S> {
  type Value = ConstInputValue<S>;

  #[inline]
  fn field_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn field_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn field_value(&self) -> &Self::Value {
    self.value()
  }
}

impl<S> ArgumentLike<S> for Argument<S> {
  type Value = InputValue<S>;

  #[inline]
  fn argument_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn argument_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn argument_value(&self) -> &Self::Value {
    self.value()
  }
}

impl<S> ArgumentLike<S> for ConstArgument<S> {
  type Value = ConstInputValue<S>;

  #[inline]
  fn argument_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn argument_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn argument_value(&self) -> &Self::Value {
    self.value()
  }
}

impl<S> ValueLike<S> for InputValue<S> {
  type Field = ObjectField<S>;

  #[inline]
  fn value_span(&self) -> SimpleSpan {
    *self.as_span()
  }

  #[inline]
  fn as_variable(&self) -> Option<&VariableValue<S>> {
    match self {
      Self::Variable(variable) => Some(variable),
      _ => None,
    }
  }

  #[inline]
  fn is_null(&self) -> bool {
    matches!(self, Self::Null(_))
  }

  #[inline]
  fn as_boolean(&self) -> Option<&BooleanValue<S>> {
    match self {
      Self::Boolean(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_int(&self) -> Option<&IntValue<S>> {
    match self {
      Self::Int(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_float(&self) -> Option<&FloatValue<S>> {
    match self {
      Self::Float(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_string(&self) -> Option<&StringValue<S>> {
    match self {
      Self::String(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_enum(&self) -> Option<&EnumValue<S>> {
    match self {
      Self::Enum(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_list(&self) -> Option<&[Self]> {
    match self {
      Self::List(list) => Some(list.values()),
      _ => None,
    }
  }

  #[inline]
  fn as_object(&self) -> Option<&[ObjectField<S>]> {
    match self {
      Self::Object(object) => Some(object.fields()),
      _ => None,
    }
  }
}

impl<S> ValueLike<S> for ConstInputValue<S> {
  type Field = ConstObjectField<S>;

  #[inline]
  fn value_span(&self) -> SimpleSpan {
    *self.as_span()
  }

  #[inline]
  fn as_variable(&self) -> Option<&VariableValue<S>> {
    None
  }

  #[inline]
  fn is_null(&self) -> bool {
    matches!(self, Self::Null(_))
  }

  #[inline]
  fn as_boolean(&self) -> Option<&BooleanValue<S>> {
    match self {
      Self::Boolean(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_int(&self) -> Option<&IntValue<S>> {
    match self {
      Self::Int(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_float(&self) -> Option<&FloatValue<S>> {
    match self {
      Self::Float(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_string(&self) -> Option<&StringValue<S>> {
    match self {
      Self::String(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_enum(&self) -> Option<&EnumValue<S>> {
    match self {
      Self::Enum(value) => Some(value),
      _ => None,
    }
  }

  #[inline]
  fn as_list(&self) -> Option<&[Self]> {
    match self {
      Self::List(list) => Some(list.values()),
      _ => None,
    }
  }

  #[inline]
  fn as_object(&self) -> Option<&[ConstObjectField<S>]> {
    match self {
      Self::Object(object) => Some(object.fields()),
      _ => None,
    }
  }
}

/// A GraphQL directive application, whether or not its arguments may contain variables.
pub(crate) trait DirectiveLike<S> {
  /// The argument row type this directive holds.
  type Argument: ArgumentLike<S>;

  /// Returns the span covering the directive.
  fn directive_span(&self) -> SimpleSpan;
  /// Returns the directive's name, without the `@`.
  fn directive_name(&self) -> &Name<S>;
  /// Returns the directive's arguments; empty when it has none.
  fn directive_arguments(&self) -> &[Self::Argument];
}

impl<S> DirectiveLike<S> for Directive<S> {
  type Argument = Argument<S>;

  #[inline]
  fn directive_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn directive_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn directive_arguments(&self) -> &[Self::Argument] {
    match self.arguments() {
      Some(arguments) => arguments.arguments(),
      None => &[],
    }
  }
}

impl<S> DirectiveLike<S> for ConstDirective<S> {
  type Argument = ConstArgument<S>;

  #[inline]
  fn directive_span(&self) -> SimpleSpan {
    *self.span()
  }

  #[inline]
  fn directive_name(&self) -> &Name<S> {
    self.name()
  }

  #[inline]
  fn directive_arguments(&self) -> &[Self::Argument] {
    match self.arguments() {
      Some(arguments) => arguments.arguments(),
      None => &[],
    }
  }
}

// ---------------------------------------------------------------------------------------------
// document accessors
// ---------------------------------------------------------------------------------------------

/// Returns a definition by index, stripped of the description wrapper.
///
/// Descriptions on executable definitions are documentation and nothing else — the spec says
/// they "MUST NOT affect the execution, validation, or response of a GraphQL document" — so no
/// rule ever sees one. `tests/validator_descriptions.rs` holds that as a verdict equivalence.
#[inline]
pub(crate) fn definition<S>(
  document: &ExecutableDocument<S>,
  index: u32,
) -> Option<&ExecutableDefinition<S>> {
  document
    .definitions()
    .get(index as usize)
    .map(|described| described.node())
}

/// Returns the operation at `index`, or `None` when the definition is a fragment.
#[inline]
pub(crate) fn operation<S>(
  document: &ExecutableDocument<S>,
  index: u32,
) -> Option<&OperationDefinition<S>> {
  match definition(document, index)? {
    ExecutableDefinition::Operation(operation) => Some(operation),
    ExecutableDefinition::Fragment(_) => None,
  }
}

/// Returns the fragment at `index`, or `None` when the definition is an operation.
#[inline]
pub(crate) fn fragment<S>(
  document: &ExecutableDocument<S>,
  index: u32,
) -> Option<&FragmentDefinition<S>> {
  match definition(document, index)? {
    ExecutableDefinition::Fragment(fragment) => Some(fragment),
    ExecutableDefinition::Operation(_) => None,
  }
}

/// Returns a definition's own top-level selection set.
#[inline]
pub(crate) fn root_selection_set<S>(
  document: &ExecutableDocument<S>,
  index: u32,
) -> Option<&SelectionSet<S>> {
  Some(match definition(document, index)? {
    ExecutableDefinition::Operation(OperationDefinition::Named(named)) => named.selection_set(),
    ExecutableDefinition::Operation(OperationDefinition::Shorthand(set)) => set,
    ExecutableDefinition::Fragment(fragment) => fragment.selection_set(),
  })
}

/// Returns the selection set a selection encloses, when it encloses one.
///
/// A fragment spread never does — its body belongs to the definition it names, which is why the
/// traversal enters a spread by starting a new definition level rather than by descending.
#[inline]
pub(crate) fn child_selection_set<S>(selection: &Selection<S>) -> Option<&SelectionSet<S>> {
  match selection {
    Selection::Field(field) => field.selection_set(),
    Selection::InlineFragment(inline) => Some(inline.selection_set()),
    Selection::FragmentSpread(_) => None,
  }
}

/// Returns a name's source bytes.
#[inline]
pub(crate) fn name_bytes<S>(name: &Name<S>) -> &[u8]
where
  S: AsRef<[u8]>,
{
  name.source().as_ref()
}

/// Returns a field's response name — the alias when it has one, otherwise the field name.
#[inline]
pub(crate) fn response_name<S>(field: &crate::parser::graphql::ast::Field<S>) -> &Name<S> {
  match field.alias() {
    Some(alias) => alias.name(),
    None => field.name(),
  }
}
