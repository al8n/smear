#!/bin/bash

# object_definition.rs
cat > object_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoComponents, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of object type definition: `type Name implements Interface { fields }`
#[derive(Debug, Clone)]
pub struct ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  type_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn type_keyword_padding(&self) -> &Padding<S, TriviaContainer> { &self.type_keyword_padding }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn implements(&self) -> Option<&Implements> { self.implements.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn fields_definition(&self) -> Option<&Fields> { self.fields_definition.as_ref() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for ObjectTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}

/// Object extension data
#[derive(Debug, Clone)]
pub enum ObjectTypeExtensionData<Implements, Directives, Fields> {
  Directives { implements: Option<Implements>, directives: Directives },
  Fields { implements: Option<Implements>, directives: Option<Directives>, fields: Fields },
  Implements(Implements),
}

impl<Implements, Directives, Fields> ObjectTypeExtensionData<Implements, Directives, Fields> {
  pub const fn implements(&self) -> Option<&Implements> {
    match self {
      Self::Directives { implements, .. } | Self::Fields { implements, .. } => implements.as_ref(),
      Self::Implements(i) => Some(i),
    }
  }

  pub const fn directives(&self) -> Option<&Directives> {
    match self {
      Self::Directives { directives, .. } => Some(directives),
      Self::Fields { directives, .. } => directives.as_ref(),
      Self::Implements(_) => None,
    }
  }

  pub const fn fields_definition(&self) -> Option<&Fields> {
    match self {
      Self::Fields { fields, .. } => Some(fields),
      _ => None,
    }
  }
}

/// CST representation of object type extension
#[derive(Debug, Clone)]
pub struct ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  extend_keyword_padding: Padding<S, TriviaContainer>,
  type_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  data: ObjectTypeExtensionData<Implements, Directives, Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn data(&self) -> &ObjectTypeExtensionData<Implements, Directives, Fields> { &self.data }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for ObjectTypeExtension<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# interface_definition.rs (similar to object)
cat > interface_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of interface type definition
#[derive(Debug, Clone)]
pub struct InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  interface_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  implements: Option<Implements>,
  directives: Option<Directives>,
  fields_definition: Option<Fields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer>
  InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn implements(&self) -> Option<&Implements> { self.implements.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn fields_definition(&self) -> Option<&Fields> { self.fields_definition.as_ref() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> AsSpan<Span>
  for InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Implements, Directives, Fields, S, TriviaContainer> IntoSpan<Span>
  for InterfaceTypeDefinition<Name, Implements, Directives, Fields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# union_definition.rs
cat > union_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of union type definition: `union Name = Type1 | Type2`
#[derive(Debug, Clone)]
pub struct UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  union_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
  union_member_types: Option<UnionMembers>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, UnionMembers, Directives, S, TriviaContainer>
  UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn union_member_types(&self) -> Option<&UnionMembers> { self.union_member_types.as_ref() }
}

impl<Name, UnionMembers, Directives, S, TriviaContainer> AsSpan<Span>
  for UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, UnionMembers, Directives, S, TriviaContainer> IntoSpan<Span>
  for UnionTypeDefinition<Name, UnionMembers, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# input_object_definition.rs
cat > input_object_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of input object type definition
#[derive(Debug, Clone)]
pub struct InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  input_keyword_padding: Padding<S, TriviaContainer>,
  name: Name,
  directives: Option<Directives>,
  input_fields_definition: Option<InputFields>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Name, Directives, InputFields, S, TriviaContainer>
  InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn name(&self) -> &Name { &self.name }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn input_fields_definition(&self) -> Option<&InputFields> { self.input_fields_definition.as_ref() }
}

impl<Name, Directives, InputFields, S, TriviaContainer> AsSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Name, Directives, InputFields, S, TriviaContainer> IntoSpan<Span>
  for InputObjectTypeDefinition<Name, Directives, InputFields, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# schema_definition.rs
cat > schema_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of schema definition
#[derive(Debug, Clone)]
pub struct SchemaDefinition<Directives, OperationTypes, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  schema_keyword_padding: Padding<S, TriviaContainer>,
  directives: Option<Directives>,
  operation_types: OperationTypes,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Directives, OperationTypes, S, TriviaContainer>
  SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn operation_types(&self) -> &OperationTypes { &self.operation_types }
}

impl<Directives, OperationTypes, S, TriviaContainer> AsSpan<Span>
  for SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Directives, OperationTypes, S, TriviaContainer> IntoSpan<Span>
  for SchemaDefinition<Directives, OperationTypes, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# operation_definition.rs
cat > operation_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of operation definition (query, mutation, subscription)
#[derive(Debug, Clone)]
pub struct OperationDefinition<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  operation_type: Option<OperationType>,
  name: Option<Name>,
  variable_definitions: Option<Variables>,
  directives: Option<Directives>,
  selection_set: SelectionSet,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
  OperationDefinition<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn operation_type(&self) -> Option<&OperationType> { self.operation_type.as_ref() }
  pub const fn name(&self) -> Option<&Name> { self.name.as_ref() }
  pub const fn variable_definitions(&self) -> Option<&Variables> { self.variable_definitions.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn selection_set(&self) -> &SelectionSet { &self.selection_set }
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer> AsSpan<Span>
  for OperationDefinition<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer> IntoSpan<Span>
  for OperationDefinition<OperationType, Name, Variables, Directives, SelectionSet, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# fragment_definition.rs
cat > fragment_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of fragment definition
#[derive(Debug, Clone)]
pub struct FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  fragment_keyword_padding: Padding<S, TriviaContainer>,
  fragment_name: FragmentName,
  type_condition: TypeCondition,
  directives: Option<Directives>,
  selection_set: SelectionSet,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
  FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn fragment_name(&self) -> &FragmentName { &self.fragment_name }
  pub const fn type_condition(&self) -> &TypeCondition { &self.type_condition }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
  pub const fn selection_set(&self) -> &SelectionSet { &self.selection_set }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer> AsSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer> IntoSpan<Span>
  for FragmentDefinition<FragmentName, TypeCondition, Directives, SelectionSet, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

# variable_definition.rs
cat > variable_definition.rs << 'EOF'
use logosky::utils::{AsSpan, IntoSpan, Span};
use core::marker::PhantomData;
use std::vec::Vec;

use crate::cst::Padding;

/// CST representation of variable definition: `$var: Type = default`
#[derive(Debug, Clone)]
pub struct VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer = Vec<crate::cst::Trivia<S>>> {
  span: Span,
  variable: Variable,
  colon_padding: Padding<S, TriviaContainer>,
  ty: Type,
  default_value: Option<DefaultValue>,
  directives: Option<Directives>,
  _marker: PhantomData<(S, TriviaContainer)>,
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
  VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  pub const fn span(&self) -> &Span { &self.span }
  pub const fn variable(&self) -> &Variable { &self.variable }
  pub const fn colon_padding(&self) -> &Padding<S, TriviaContainer> { &self.colon_padding }
  pub const fn ty(&self) -> &Type { &self.ty }
  pub const fn default_value(&self) -> Option<&DefaultValue> { self.default_value.as_ref() }
  pub const fn directives(&self) -> Option<&Directives> { self.directives.as_ref() }
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer> AsSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn as_span(&self) -> &Span { self.span() }
}

impl<Variable, Type, DefaultValue, Directives, S, TriviaContainer> IntoSpan<Span>
  for VariableDefinition<Variable, Type, DefaultValue, Directives, S, TriviaContainer>
{
  fn into_span(self) -> Span { self.span }
}
EOF

echo "All major definition files created"
